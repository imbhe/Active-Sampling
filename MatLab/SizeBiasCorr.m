function [SBCProb] = SizeBiasCorr(GlanceDistIn, bPlot)
% [w] = SizeBiasCorr(GlanceDistIn, bPlot)
%
% SizeBiasCorr  calculates size-bias corrected probabilities for the EOFF 
%               histogram or pmf as input.
%
%   Function input:
%
%     GlanceDistIn - is a eyes off-road (EOFF) distribution with a point-mass
%                   at zero representing the percent eyes-on-road (EON) for
%                   the task or baseline. GlanceDistIn(:,1) is the time vector,
%                   while GlanceDistIn(:,2) is the counts in a histogram or
%                   probabilities. 
%
%     bPlot - One (1) if a graph should be shown. Zero (0) it no graph should be shown.
%
%  Function output:
%
%     SBC_Prob - Size-bias corrected (SBC) probabilities from EOFF input.
%
%  Other m-files required: none
%
%  MAT or other files required: none

%
%  Author(s): Jonas Bärgman and Henrik Imberg at
%               Chalmers University of Technology and University of Gothenburg
%               Email: jonas.bargman@chalmers.se, imbergh@chalmers.se
%

%%%%%%%%%%%%%%%%%%%%
% REVISION HISTORY %
%%%%%%%%%%%%%%%%%%%%
% 1.00, 2020-03-09, Henrik Imberg: First version.

SCRIPT_VERSION = '1.00';

%------------- BEGIN CODE --------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simplify notation.
t = GlanceDistIn(:, 1); % Glance times.
f = GlanceDistIn(:, 2); % Frequencies.

% Size-bias corrected probability proportional to time proportion with glances 
% of certain length.
p = t .* f; 
p = p / sum(p); % Normalize, sum up to one. 

% Correction for eyes-on-road (zero glance times).
if any(t == 0)
  % Probability mass at zero.
   p(t == 0) = f(t == 0) / sum(f); 
   
   % Re-weight non-zero glance times.
   p(t ~= 0) = (1 - f(t == 0) / sum(f)) .* p(t ~= 0); 
end

SBCProb = p; % Rename. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Figure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if bPlot
    % Show input glance distribution and size-bias corrected probabilities.
    
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
    title('Size-bias corrected glance distribution');
    xlabel('EOFF duration [s]');
    ylabel('Size-bias corrected probability');
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
    xlabel('EOFF duration [s]');
    ylabel('Size-bias corrected probability');
    xlim([0,7]);
    hold off;
 
end