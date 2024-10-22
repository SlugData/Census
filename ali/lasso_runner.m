load('census_v4.mat')
header = header{1};
mail_return_rate = training_data(:,169);
training_data(:,169) = [];
any(~isnan(training_data(:,6)))
training_data(:,6) = [];

[B,FitInfo] = lasso(training_data,mail_return_rate,'CV',5);

lassoPlot(B,FitInfo,'PlotType','lambda')
lassoPlot(B,FitInfo,'PlotType','CV')
lassoPlot(B,FitInfo,'PlotType','L1')

feature_name{find(B(:,87))}
