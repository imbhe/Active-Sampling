function [MXR] = MXR(RiskIn,GlanceDistIn,bPlot)
% [MXR] = MXR(RiskIn, GlanceDistIn, bPlot)
% 
% MXR calculates the MIR or MCR as described in 
%   Bärgman et al., (2015). This function was supplied as additional material
%   to the Bärgman et al., (2015) paper. The understanding of the descriptions 
%   in this function requires access to the definitons desribed in that paper.
%
%   Function input:
%
%   RiskIn - is the injury risk or crash probablity of an event (or the mean across a set
%                   of events) as a function of time. The time is offset to be 
%                   zero at the chosen glance anchor point. This script was 
%                   supplied with three sets of mean injury risk curves and
%                   three crash probability curves. The curves were 
%                   the mean injury risk across the crash, near-crash and pooled 
%                   near-crash, as well as crash events respectively, as 
%                   used in the Bärgman et al., (2015), paper. The time of the
%                   injury risk curves are offset to zero at the time of inverse tau 
%                   being greater than 0.1 (invTau=>0.1) just prior the crash
%                   or near-crash in each event. The RiskIn(:,1) is the
%                   time vector, while RiskIn(:,2) are the risk or
%                   probability values. Any risk function can be used instead of
%                   the supplied curves. However, note that this function
%                   assumes a 0.1s time step for the injury risk function.
%
%   GlanceDistIn - is a eyes off-road (EOFF) distribution with a point-mass 
%                   at zero representing the percent eyes-on-road (EON) for
%                   the task or baseline. GlanceDistIn(:,1) is the time vector,  
%                   while GlanceDistIn(:,2) is the counts in a histogram or 
%                   probabilities. Note that this function is written assuming 
%                   a time step of 0.1 and with all times from zero (0)
%                   until the longest duration bins. That is, the bin
%                   centers are at 0, 0.1, 0.2....
%                   
%   bPlot - One (1) if a graph should be shown. Zero (0) it no graph should be shown. 
%                   Note that the curves in the plot are normalized for
%                   the purpose of visualization and thus do not reflect the
%                   actual magnutude of the respective profiles, only their
%                   shapes.
%
% Function output:
%
%   MIR - Is the injury risk metric as described in Bärgman et al, (2015). That
%                   is, it is integral(h(t)*r(t) over all times, where h(t) is
%                   the GlanceDistIn transformed to an overshot PDF and
%                   r(t) is the risk. Note that, given this is a generalized 
%                   functioni for calcualtion of both MIR and MCR, we use the 
%                   notation r(t) here for the calculation of both MIR and MCR, 
%                   even if the notation for MCR is c(t) in the Bärgman et. al, 
%                   (2015) paper.  
%
%   Other m-files required: none
%
%   MAT or other files required: none
%
%
%   Author(s): Jonas Bärgman and Vera Liskovskaja at Chalmers University
%               of Technology. 
%               Email: jonas.bargman@chalmers.se, vera@chalmers.se  
%

%%%%%%%%%%%%%%%%%%%%
% REVISION HISTORY %
%%%%%%%%%%%%%%%%%%%%
% 1.00, 2014-12-18, Jonas Bärgman: First production version
% 2.00, 2015-07-09, Jonas Bärgman: Major structural revision before final submission 

SCRIPT_VERSION = '2.00';

%------------- BEGIN CODE --------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The speed profiles and risk functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
% Assumming 0.1s time step in the data, shift 4 steps to produce a 0.4s
%   response  delay to simulate driver reaction time after having the 
%   eyes back on the forward roadway. See Bärgman et al., (2015) for
%   the rationale. From here on DelayData is glance distribution shifted
%   with reaction time.
iTimeShiftIndices = 0;   % This value could be changed by the user/reader. 4 corresponds to a reaction time of 0.4 s. 
fTimeStep = 0.1;

fDelayData = [zeros(1,iTimeShiftIndices), GlanceDistIn(:,2)'];
fDelayDataTime = (0:length(fDelayData)-1)*fTimeStep;

%% Simplify notation

x = fDelayDataTime'; % Possible lengths of glances plus the reaction time
t = RiskIn(:,1); % The risk time

IR = RiskIn(:,2); % Injury risk (if MIR is calculated) or crash probability (if MCR is calculated). 

eps = 10^-4; %% Just a correction, needed later

%% The x-axis matching/extension. 
% Cuts the risk profile at the time of the longest duration of glance+reaction 
% time. Alternatevely, if the risk is defined for shorter time values than
% the glances, uses the last value of the risk function as input for the
% missing "tail".

if x(end) - t(end) > 0 % If longer, extend the last values
    add_num = round((x(end) - t(end))/(x(2)-x(1)));
    t = [t(1:end);t(end)+((x(2)-x(1))*(1:add_num))'];
    IR = [IR(1:end); IR(end)*ones(add_num,1)];    
elseif x(end) - t(end) < 0 % If shorter, cut off the last values
    t = t(1:length(x));
    IR = IR(1:length(x));
end

if (sum(t-x) > eps)
    print('Error: The x-axis for the glances and the risk function do not match')
end

clear t

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculation of the MIR / MCR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create the size-bias corrected overshot PDF of the glance distribution 
%   + reaction time added 

% The original empirical distribution
f = fDelayData'/sum(fDelayData); % The empirical frequences for each length. Display by e.g. plot(x,f)

% The distribution of the time from a (potential) event to the reaction
% time. Not normalized, so does not sum up to 1.
h = (fliplr(cumsum(fliplr(f')))/(x'*f))';

% h as a continuous distribution function on x: x > 0
h_f = @(t,x)sum( (t >= x(1:end-1) & t < x(2:end) == 1).*h(2:end));

% If there is a point mass at zero in the original glance distribution,
% re-weight the distribution to create a distribution mixture with point
% mass at 0
if f(x == 0) > 0
    f0 = f(x==0);
    h_f = @(t,x)(1-f0).*h_f(t,x);
end

%% Approximate R with piecewise linear function

pp = interp1(x,IR,'linear','pp');
R_f = @(T)ppval(pp,T);

%% Create the h(t)'r(t) function and use it to calculate MIR or MCR
Int_R = @(T)h_f(T,x).*R_f(T);
MXR = quadv(Int_R,0,x(end)-eps);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the data if requested
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if bPlot
    % Note that these plots are normalized and will not thus not be
    % identical to those in the Bärgman et. al, (2015) paper.
    figure(1)
    clf;
    T2 = linspace(0,max(x)-eps,1000);
    plot(T2,arrayfun(@(t)h_f(t,x),T2)/max(arrayfun(@(t)h_f(t,x),T2)),'b','linewidth',3)
    hold on;
    plot(T2,arrayfun(@(t)R_f(t),T2)/max(arrayfun(@(t)R_f(t),T2)),'k','linewidth',3)
    plot(T2,arrayfun(@(t)Int_R(t),T2)/max(arrayfun(@(t)Int_R(t),T2))*0.5,'m','linewidth',3)
    
    title(['h(t) based on EOFF, r(t) risk (MIR or MCR), and h(t)*r(t)']);
    ylabel('Size bias corrected overshot probability')
    xlabel('EOFF duration [s]')
    xlim([0,7])
    
    legend({'h(t)','r(t)','h(t)*r(t)'})
end

%% Code added by Henrik Imberg, March 4th 2020

% Probability mass function of (discrete) overshot distribution, conditionally 
% on overshot time being strictly greater than zero.
pmf_wrong = h(2:end) .* f(2:end); % Wrong.
pmf_wrong = pmf_wrong / sum(pmf_wrong);
pmf = h(2:end) / sum(h(2:end)); % Correct.

fig = figure('Position', [200, 200, 960, 360]);
  subplot(1, 3, 1);
    bar(x(2:end), f(2:end), 'b');
    hold on;
    title('Glance distribution');
    xlabel('EOFF duration [s]');
    ylabel('Probability');
    xlim([0,7]) 
  subplot(1, 3, 2);
    bar(x(2:end), pmf_wrong, 'b');
    title('Overshot distribution (wrong)'); 
    xlabel('EOFF duration [s]');
    ylabel('Probability');    
    xlim([0,7])
  subplot(1, 3, 3);
    bar(x(2:end), pmf, 'b');
    title('Overshot distribution (correct)'); 
    xlabel('EOFF duration [s]');
    ylabel('Probability');
    xlim([0,7]);
 
hold off;

saveas(fig, "OvershotDist.png");
