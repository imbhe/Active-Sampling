function [Overshot] = EOFF2Overshot(GlanceDistIn,bPlot)
% [Overshot] = EOFF2Overshot(GlanceDistIn,bPlot)
%
% EOFF2Overshot calculates the overshot distribution of the EOFF histogram
%                   or pdf as input.
%
%   Function input:
%
%     GlanceDistIn - is a eyes off-road (EOFF) distribution with a point-mass
%                   at zero representing the percent eyes-on-road (EON) for
%                   the task or baseline. GlanceDistIn(:,1) is the time vector,
%                   while GlanceDistIn(:,2) is the counts in a histogram or
%                   probabilities. Note that this function is written assuming
%                   a time step of 0.1 and with all times from zero (0)
%                   until the longest duration bins. That is, the bin
%                   centers are at 0, 0.1, 0.2....
%
%     bPlot - One (1) if a graph should be shown. Zero (0) it no graph should be shown.
%
%  Function output:
%
%     Overshot - The overshot distribution of the EOFF input.
%
%  Other m-files required: none
%
%  MAT or other files required: none
%
%
%  Author(s): Jonas Bärgman, Vera Liskovskaja, and Henrik Imberg at
%               Chalmers University of Technology and University of Gothenburg
%               Email: jonas.bargman@chalmers.se, vera@chalmers.se, imbergh@chalmers.se
%

%%%%%%%%%%%%%%%%%%%%
% REVISION HISTORY %
%%%%%%%%%%%%%%%%%%%%
% 1.00, 2014-12-18, Jonas Bärgman: First production version
% 2.00, 2015-07-09, Jonas Bärgman: Major structural revision before final submission
% 3.00, 2020-03-08, Henrik Imberg and Jonas Bärgman: Transformed from MXR (risk)
%                   to a pure descrete overshot distribution of EOFFs only
%                   as input. 
% 4.00, 2020-03-09, Henrik Imberg: Zero glance time included in overshot distribution.
%                   Reaction time excluded from overshot distribution.                  

SCRIPT_VERSION = '4.00';

%------------- BEGIN CODE --------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simplify notation.
t = GlanceDistIn(:, 1); % Glance times.
f = GlanceDistIn(:, 2); % Frequencies.
t_gt0 = t(t > 0); % Glance times greater than zero.
f_tgt0 = f(t > 0); % Frequencies of glance times greater than zero.


% Overshot distribution. The normalized disribution of overshots of EOFF
% (over anchor point, e.g. invtau=0.2).
p = flip(cumsum(flip(f_tgt0)));
p = p / sum(p); % Normalize, sum up to one. 


% Correction for eyes-on-road (zero glance time -> no overshot time).
if any(t == 0)
  
  % Probability mass at zero.
  p = [0; p(:)];
  p(1) = f(t == 0) / sum(f); 
   
  % Re-weight non-zero glance times.  
  p(2:end) = (1 - p(1)) .* p(2:end); 
  
  % Add zero glance time to time vector.
  t = [0; t_gt0(:)];
end

Overshot = p; % Rename.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Figure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if bPlot
    % Show input glance distribution and overshot distribution.
    
    figure('Position', [200, 200, 960, 360]);
    subplot(2, 2, 1);
    bar(t, f / sum(f), 'b');
    hold on;
    title('Input glance distribution');
    xlabel('EOFF duration [s]');
    ylabel('Relative frequency');
    xlim([0,7])
    
    subplot(2, 2, 2);
    bar(t, p, 'b');
    title('Overshot distribution');
    xlabel('EOFF duration [s] (Overshot time)');
    ylabel('Probability');
    xlim([0,7]);
    
    subplot(2, 2, 3);
    bar(t(t>0), f(t>0) / sum(f), 'b');
    title('Zooming in: t > 0');
    xlabel('EOFF duration [s]');
    ylabel('Relative frequency');
    xlim([0,7]);
    
    subplot(2, 2, 4);
    bar(t(t>0), p(t>0), 'b');
    title('Zooming in: t > 0');
    xlabel('EOFF duration [s] (Overshot time)');
    ylabel('Probability');
    xlim([0,7]);
    hold off;
 
end