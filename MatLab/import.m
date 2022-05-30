close all
clear all
clc

% Read data.
RiskIn = csvread('./../Data/MeanInjRiskNC_C.csv');
GlanceDistIn = csvread('./../Data/BaselineGlanceDist.csv');

Overshot = EOFF2Overshot(GlanceDistIn, 1);
SBCProb = SizeBiasCorr(GlanceDistIn, 1);

t = zeros(sum(GlanceDistIn(:,2)), 1);
ix = 1;
for i = 1:length(GlanceDistIn(:,2))
  t(ix:(ix + GlanceDistIn(i,2) - 1)) = GlanceDistIn(i, 1);
end