%Read detector output
Detects = readtable( 'LFDCS_AWI247-02_SV1008_786edit.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);

%Read monthly
Nov = readtable( 'LFDCS_AWI247-02_SV1008_Nov.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Dec = readtable( 'LFDCS_AWI227-12_SV1025_201212.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Jan = readtable( 'LFDCS_AWI227-12_SV1025_201301.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Feb = readtable( 'LFDCS_AWI227-12_SV1025_201302.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Mar = readtable( 'LFDCS_AWI227-12_SV1025_201303.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Apr = readtable( 'LFDCS_AWI227-12_SV1025_201304.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
May = readtable( 'LFDCS_AWI227-12_SV1025_201305.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Jun = readtable( 'LFDCS_AWI227-12_SV1025_201306.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Jul = readtable( 'LFDCS_AWI227-12_SV1025_201307.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
Aug = readtable( 'LFDCS_AWI247-02_SV1008_Aug.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
                   
%Read months by loop
dinfo = dir('/Users/eschall/Documents/MATLAB/LFDCS/LFDCS_AWI229-09_SV1000/*/*.csv');    %Fileinfo of audiofiles to edit
File_names = {dinfo.name};  %Create list of file names
Paths_in = {dinfo.folder};  %Create list of path names
Detects = table();
for i = 1:length(dinfo)
    tab = readtable( [Paths_in{i} '/' File_names{i}], ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
    Detects = [Detects;tab];
end
                   
%Combine months
Detects = [Dec;Jan;Feb;Mar;Apr;Jun;Jul];
                   
  %Change to date/time format
    sDate = repmat(datetime(2012,12,11,09,10,01),height(Mar),1);
    Mar.DateTime = sDate + seconds(Mar.Var2);
    Mar.MarDate =  datenum(Mar.DateTime);
    date = [Mar.DateTime.Year, Mar.DateTime.Month, Mar.DateTime.Day];
    Mar.Date = datetime(date);
    Mar.Hour = [Mar.DateTime.Hour];
    days_hours = [Mar.DateTime.Year, Mar.DateTime.Month, Mar.DateTime.Day, Mar.DateTime.Hour, zeros(height(Mar),1), zeros(height(Mar),1)];
    Mar.DateHour = datetime(days_hours);
    
    sDate = repmat(datetime(2010,12,15,08,50,01),height(Detects),1);
    Detects.DateTime = sDate + seconds(Detects.Var2);
    Detects.JulDate =  datenum(Detects.DateTime);
    date = [Detects.DateTime.Year, Detects.DateTime.Month, Detects.DateTime.Day];
    Detects.Date = datetime(date);
    Detects.Hour = [Detects.DateTime.Hour];
    days_hours = [Detects.DateTime.Year, Detects.DateTime.Month, Detects.DateTime.Day, Detects.DateTime.Hour, zeros(height(Detects),1), zeros(height(Detects),1)];
    Detects.DateHour = datetime(days_hours);
    
    %Detects = [Detects;Dec];
    %Detects = sortrows(Detects,11);

%Filter call types
Detects(Detects.Var1>=20 & (Detects.Var9 > 2 | Detects.Var8 <14),:) = [];
day_hour = unique(Detects.DateHour);
CR_tO = 5;
for i = 1:length(day_hour)
    Detects_f = Detects(Detects.DateHour == day_hour(i),:);
if sum(Detects_f.Var1 == 30) >= CR_tO && ...
         sum(Detects_f.Var9 <= 2 & Detects_f.Var8 >= 14 & (Detects_f.Var1 == 1 | Detects_f.Var1 == 3 | ...
        Detects_f.Var1 == 4 | Detects_f.Var1 == 5 | Detects_f.Var1 == 6)) <= CR_tO
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = []; end
if sum(Detects_f.Var1 == 31) >= CR_tO && ...
        sum(Detects_f.Var9 <= 2 & Detects_f.Var8 >= 14 & (Detects_f.Var1 == 1 | Detects_f.Var1 == 3 | ...
        Detects_f.Var1 == 4 | Detects_f.Var1 == 5 | Detects_f.Var1 == 6)) <= CR_tO
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = []; end
if sum(Detects_f.Var1 == 32) >= CR_tO && ...
        sum(Detects_f.Var9 <= 2 & Detects_f.Var8 >= 14 & (Detects_f.Var1 == 1 | Detects_f.Var1 == 3 | ...
        Detects_f.Var1 == 4 | Detects_f.Var1 == 5 | Detects_f.Var1 == 6)) <= CR_tO
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = []; end
if sum(Detects_f.Var1 == 34 | Detects_f.Var1 == 35) >= 1.5*CR_tO && ...
        sum(Detects_f.Var9 <= 2 & Detects_f.Var8 >= 14 & (Detects_f.Var1 == 1 | Detects_f.Var1 == 3 | ...
        Detects_f.Var1 == 4 | Detects_f.Var1 == 5 | Detects_f.Var1 == 6)) <= CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 1,:) = [];
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 6,:) = []; end
if sum(Detects_f.Var1 == 36) >= CR_tO && ...
        sum(Detects_f.Var9 <= 2 & Detects_f.Var8 >= 14 & (Detects_f.Var1 == 1 |  ...
        Detects_f.Var1 == 4 | Detects_f.Var1 == 5 | Detects_f.Var1 == 6)) <= CR_tO
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 3,:) = []; end
end
Detects = Detects(Detects.Var1<=19,:);


%Calculate DCS-observed daily call rates for 2.5 Mdist and 13 SNR thresholds
Mdist = 2.5;
SNR = 13;
Det_hours = table(unique(Detects.DateHour),zeros(size(unique(Detects.DateHour),1),1),...
    zeros(size(unique(Detects.DateHour),1),1),zeros(size(unique(Detects.DateHour),1),1),...
    zeros(size(unique(Detects.DateHour),1),1),zeros(size(unique(Detects.DateHour),1),1),...
    'VariableNames', {'DateHour', 'Detections', 'Presence','Song','Social','Quality'});

Detects_f = Detects(Detects.Var9<=Mdist,:);
Detects_ff = Detects_f(Detects_f.Var8>=SNR,:);
        for i = 1:height(Det_hours)
            if unique(Detects_ff.Var1(Detects_ff.DateHour == Det_hours.DateHour(i))) == 18
                Detects_ff(Detects_ff.DateHour == Det_hours.DateHour(i),:) = [];
            end
            Det_hours(i,2) = {height(Detects_ff(Detects_ff.DateHour == Det_hours.DateHour(i),:))};
        end  
        
writetable(Det_hours,'AWI229-09_SV1000_HWhourly.csv')







%Calculate probability of presence and probability of FN hours per
%DCS-observed hourly call rate threshold
CR_t = [10:1000];
comb = struct('MdistSNR',GT_d.Properties.VariableNames(1,7:55),'CR_t',CR_t,...
    'ProbOfPres',zeros(1,length(CR_t)),'ProbOfFN',zeros(1,length(CR_t)));
c = 0;
ProbOfPres = zeros(1,length(CR_t));
ProbOfFN = zeros(1,length(CR_t));

for j = 7:size(GT_d,2)
    c = c+1;
    for i = 1:length(CR_t)
        GTlargerCRt = GT_d(table2array(GT_d(:,j)) >= CR_t(i),[1:5,j]);
        GTsmallerCRt = GT_d(table2array(GT_d(:,j)) < CR_t(i),[1:5,j]);
        ProbOfPres(i) = height(GTlargerCRt(GTlargerCRt.H == 1,:)) / height(GTlargerCRt);
        ProbOfFN(i) = height(GTsmallerCRt(GTsmallerCRt.H == 1,:)) / height(GTsmallerCRt);
        %MissRate=ProbOfFN(i) = height(GTsmallerCRt(GTsmallerCRt.H == 1,:)) / height(GT_h(GT_h.H == 1,:));
    end
    comb(c).ProbOfPres = ProbOfPres;
    comb(c).ProbOfFN = ProbOfFN;
    %b = glmfit(CR_t,GT_h(:,j),'binomial','link','logit');
    %yfit = glmval(b,comb(plotID).CR_t,'logit','size',length(comb(plotID).CR_t))
end


figure(1)
for plotID = 1:49
    subplot(5,10,plotID)
    yyaxis left
    plot(comb(plotID).CR_t,comb(plotID).ProbOfPres,'.','MarkerSize', 10)
    ylim([0,1])
    ylabel('probability of presence')
    
    title(comb(plotID).MdistSNR)
    hold on
    %plot(x,yfit./length(comb(plotID).CR_t),'-','LineWidth',2)
    yyaxis right
    plot(comb(plotID).CR_t,comb(plotID).ProbOfFN,'.','MarkerSize', 10)
    ylim([0,1])
    ylabel('probability of FN hours')
    hold off
end
suplabel('LFDCS-observed hourly call rate','x',[0.08 0.1 0.84 0.84]);
suplabel('Parameter setting PO786edit','t',[0.08 0.11 0.84 0.84])

[rowFN,colFN] = find(vertcat(comb(:).ProbOfFN) <= 0.2);
LowMissedFiltered = [];
for i = 1:length(rowFN)
    LowMissedFiltered(i,1) = rowFN(i);
    LowMissedFiltered(i,2) = colFN(i);
    LowMissedFiltered(i,3) =  comb(rowFN(i)).ProbOfFN(colFN(i));
    LowMissedFiltered(i,4) = comb(rowFN(i)).ProbOfPres(colFN(i));
end
LowMissedFiltered(LowMissedFiltered(:,4) == max(LowMissedFiltered(:,4)),:)
LowMissedFiltered = array2table(LowMissedFiltered);

%Read ground truth data
GT_d2 = readtable('AWI247-2_SV1008_JB.csv','Delimiter', ['_',','],'ReadVariableNames',true,'TreatAsEmpty',[".","NA","N/A"]);

    %Change date and time format   
    GT_d2.DateTime = datetime(num2str(GT_d2.Var1),'InputFormat','yyyyMMdd');

%Calculate DCS-detected hours on daily basis
Mdist = [1.5,2,2.5,3,3.5,4,4.5];
SNR =[8,9,10,11,12,13,14];
hours = unique(Detects.DateHour);
num = zeros(length(hours),1);
%for g = 1:length(Mdist)
g=3;
    Detects_f = Detects(Detects.Var9<=Mdist(g),:);
    %for h = 1:length(SNR)
    h=7;
        Detects_ff = Detects_f(Detects_f.Var8>=SNR(h),:);
         for i = 1:length(hours)
%             if unique(Detects_ff.Var1(Detects_ff.DateHour == GT_h.DateTime(i))) == 3
%                 Detects_ff(Detects_ff.DateHour == GT_h.DateTime(i),:) = [];
%             end
            if unique(Detects_ff.Var1(Detects_ff.DateHour == hours(i))) == 18
                Detects_ff(Detects_ff.DateHour == hours(i),:) = [];
            end
            num(i) = height(Detects_ff(Detects_ff.DateHour == hours(i),:));
            %name = ['M' erase(num2str(Mdist(g)),'.') 'S' num2str(SNR(h))];
         end
        det_hour = table(hours,num);
        date = [det_hour.hours.Year, det_hour.hours.Month, det_hour.hours.Day];
        det_hour.date = datetime(date);
        GT_d2 = addvars(GT_d2,zeros(length(GT_d2.DateTime),1),'NewVariableNames',cellstr('n_hours'));
        for i = 1:length(GT_d.DateTime)
             GT_d2.n_hours(i) = length(det_hour.num(det_hour.date == GT_d2.DateTime(i) & det_hour.num >= 11));
        end
    %end4
%end

b = glmfit(GT_d2.n_hours,GT_d2.H,'binomial','link','logit');
xx = linspace(min(GT_d2.n_hours),max(GT_d2.n_hours),length(GT_d2.n_hours));
yfit = glmval(b,xx,'logit');
x = unique(GT_d2.n_hours);
y = zeros(length(x),1);
for i = 1:length(x)
   y(i) = sum(GT_d2.H(GT_d2.n_hours == x(i)))/length(GT_d2.H(GT_d.n_hours == x(i))); 
end
figure(1)
plot(x,y,'.','MarkerSize', 10)
ylim([0,1])
ylabel('probability of daily presence')
xlabel('number of DCS-detected hours')
hold on
plot(xx,yfit,'-')
hold off


