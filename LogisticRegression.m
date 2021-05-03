   
%Read detector output
Detects = readtable( '150PO679.csv', ...
                       'Delimiter', ',', ...
                       'EmptyValue', nan, ...
                       'ReadVariableNames', false);
                  
    %Change to date/time format
    sDate = repmat(datetime(2010,12,15,06,06,45),height(Detects),1);
    Detects.DateTime = sDate + seconds(Detects.Var2);
    Detects.JulDate =  datenum(Detects.DateTime);
    date = [Detects.DateTime.Year, Detects.DateTime.Month, Detects.DateTime.Day];
    Detects.Date = datetime(date);
    Detects.Hour = [Detects.DateTime.Hour];
    days_hours = [Detects.DateTime.Year, Detects.DateTime.Month, Detects.DateTime.Day, Detects.DateTime.Hour, zeros(height(Detects),1), zeros(height(Detects),1)];
    Detects.DateHour = datetime(days_hours);

%Filter call types
Detects(Detects.Var1>=20 & Detects.Var9 >= 2.5,:) = [];
day_hour = unique(Detects.DateHour);
CR_tO = 6;
for i = 1:length(day_hour)
if sum(Detects.Var1(Detects.DateHour == day_hour(i) & Detects.Var1 == 30)) >= CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = [];
elseif sum(Detects.Var1(Detects.DateHour == day_hour(i) & Detects.Var1 == 31)) >= CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = [];
elseif sum(Detects.Var1(Detects.DateHour == day_hour(i) & Detects.Var1 == 32)) >= CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 5,:) = [];
elseif sum(Detects.Var1(Detects.DateHour == day_hour(i) & Detects.Var1 == 34 ...
        | Detects.DateHour == day_hour(i) & Detects.Var1 == 35)) >= 1.5*CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 1,:) = [];
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 6,:) = [];
elseif sum(Detects.Var1(Detects.DateHour == day_hour(i) & Detects.Var1 == 36)) >= CR_tO 
    Detects(Detects.DateHour == day_hour(i) & Detects.Var1 == 3,:) = [];
end
end
Detects = Detects(Detects.Var1<=19,:);

%Read ground truth data
GT_h = readtable('TestDataLFDCS_150.csv','ReadVariableNames',true,'TreatAsEmpty',[".","NA","N/A"]);

    %Change date and time format   
    GT_h.DateTime = datetime(GT_h.Hour,'InputFormat','yyyyMMdd-HH');

%Calculate DCS-observed hourly call rates for different Mdist and SNR thresholds
Mdist = [1.5,2,2.5,3,3.5,4,4.5];
SNR =[8,9,10,11,12,13,14];
num = zeros(height(GT_h),1);

for g = 1:length(Mdist)
    Detects_f = Detects(Detects.Var9<=Mdist(g),:);
    for h = 1:length(SNR)
        Detects_ff = Detects_f(Detects_f.Var8>=SNR(h),:);
        for i = 1:length(GT_h.DateTime)
            %if unique(Detects_ff.Var1(Detects_ff.DateHour == GT_h.DateTime(i))) == 3
             %   Detects_ff(Detects_ff.DateHour == GT_h.DateTime(i),:) = [];
            %end
            %if unique(Detects_ff.Var1(Detects_ff.DateHour == GT_h.DateTime(i))) == 18
             %   Detects_ff(Detects_ff.DateHour == GT_h.DateTime(i),:) = [];
            %end
            num(i) = height(Detects_ff(Detects_ff.DateHour == GT_h.DateTime(i),:));
            name = ['M' erase(num2str(Mdist(g)),'.') 'S' num2str(SNR(h))];
        end
        GT_h = addvars(GT_h,num,'NewVariableNames',cellstr(name));
    end
end

%Calculate probability of presence and probability of FN hours per 
%DCS-observed hourly call rate threshold
CR_t = [0:30];
comb = struct('MdistSNR',GT_h.Properties.VariableNames(1,6:54),'CR_t',CR_t,...
    'ProbOfPres',zeros(1,31),'ProbOfFN',zeros(1,31));
c = 0;
ProbOfPres = zeros(1,31);
ProbOfFN = zeros(1,31);

for j = 6:size(GT_h,2)
    c = c+1;
    for i = 1:length(CR_t)
        GTlargerCRt = GT_h(table2array(GT_h(:,j)) >= CR_t(i),[1:5,j]);
        GTsmallerCRt = GT_h(table2array(GT_h(:,j)) < CR_t(i),[1:5,j]);
        ProbOfPres(i) = height(GTlargerCRt(GTlargerCRt.H == 1,:)) / height(GTlargerCRt);
        ProbOfFN(i) = height(GTsmallerCRt(GTsmallerCRt.H == 1,:)) / height(GTsmallerCRt);
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
    xlabel('LFDCS-observed hourly call rate')
    title(comb(plotID).MdistSNR)
    hold on
    %plot(x,yfit./length(comb(plotID).CR_t),'-','LineWidth',2)
    yyaxis right
    plot(comb(plotID).CR_t,comb(plotID).ProbOfFN,'.','MarkerSize', 10)
    ylim([0,1])
    ylabel('probability of FN hours')
    hold off
end

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

%Filter data set for SNR measurements
subset_missed = GTsmallerCRt(GTsmallerCRt.H == 1,:); %measure SNR for all calls from these hours

subset_det = GTlargerCRt(GTlargerCRt.H == 1,:);
ind_r = randperm(height(subset_det),height(subset_missed)); %Create random row indices to choose positive hours
subset_det_r = subset_det(ind_r,:); %hours for checking SNR of detections
subset_det_r_details = table();
for i = 1:length(subset_det_r.DateTime)
    details = Detects_ff(Detects_ff.DateHour == subset_det_r.DateTime(i),:);
    if height(details) > 3
        ind_r = randperm(height(details),3);
        details = details(ind_r,:);
    end
    subset_det_r_details = [subset_det_r_details; details];
end

%SNR estimation 
%Read in Raven tables, copy measurements all in one row per slection and
%remove duplicate rows
t_SNR_missed = readtable('C:\Users\eschall\Documents\Humpbacks\LFDCS\SelectionTableTestDataSetLFDCS_150h_SNRmissed.txt');
for i = 1:2:height(t_SNR_missed)
   inds = find(ismissing(t_SNR_missed(i,:)));
   for j = 1:length(inds)
       t_SNR_missed(i,inds(j)) = t_SNR_missed(i+1,inds(j));
   end
end
t_SNR_missed = t_SNR_missed(1:2:height(t_SNR_missed),:);

t_SNR_detected = readtable('C:\Users\eschall\Documents\Humpbacks\LFDCS\SelectionTableTestDataSetLFDCS_150h_SNRdetected.txt');
for i = 1:2:height(t_SNR_detected)
   inds = find(ismissing(t_SNR_detected(i,:)));
   for j = 1:length(inds)
       t_SNR_detected(i,inds(j)) = t_SNR_detected(i+1,inds(j));
   end
end
t_SNR_detected = t_SNR_detected(1:2:height(t_SNR_detected),:);

%estimate SNR with inband power
for i = 1:3:height(t_SNR_missed)
   dB_signal = t_SNR_missed.AvgPower_dB_(i);
   dB_noise1 = t_SNR_missed.AvgPower_dB_(i+1);
   dB_noise2 = t_SNR_missed.AvgPower_dB_(i+2);
   t_SNR_missed.SNR_AvgPower(i) = dB_signal - mean([dB_noise1,dB_noise2]);
   %t_SNR_missed.SNR_uncleaned = 10*log10(((10^(dB_signal/10)))...
    %   /(10^(mean([dB_noise1,dB_noise2])/10)));
   %t_SNR_missed.SNR_cleaned = 10*log10(((10^(dB_signal/10))-(10^(mean([dB_noise1,dB_noise2])/10)))...
    %   /(10^(mean([dB_noise1,dB_noise2])/10)));
end

for i = 1:3:height(t_SNR_detected)
   dB_signal = t_SNR_detected.AvgPower_dB_(i);
   dB_noise1 = t_SNR_detected.AvgPower_dB_(i+1);
   dB_noise2 = t_SNR_detected.AvgPower_dB_(i+2);
   t_SNR_detected.SNR_AvgPower(i) = dB_signal - mean([dB_noise1,dB_noise2]);
   am_signal = t_SNR_detected.RMSAmp_U_(i);
   am_noise1 = t_SNR_detected.RMSAmp_U_(i+1);
   am_noise2 = t_SNR_detected.RMSAmp_U_(i+2);
   %t_SNR_detected.SNR_RMSAmpli(i) = 10*log10((am_signal / mean([am_noise1,am_noise2])));
end

SNR_missed_avg = mean(t_SNR_missed.SNR_AvgPower(1:3:height(t_SNR_missed)))
SNR_missed_std = std(t_SNR_missed.SNR_AvgPower(1:3:height(t_SNR_missed)))
SNR_detected_avg = mean(t_SNR_detected.SNR_AvgPower(1:3:height(t_SNR_detected)))
SNR_detected_std = std(t_SNR_detected.SNR_AvgPower(1:3:height(t_SNR_detected)))

yyaxis right
histogram(t_SNR_detected.SNR_AvgPower(1:3:height(t_SNR_detected)),30,'FaceColor',rgb('Chocolate'),'EdgeColor','none')
ylim([0,8])
ylabel('number of detected HW vocalizations') 
hold on
yyaxis left
histogram(t_SNR_missed.SNR_AvgPower(1:3:height(t_SNR_missed)),25,'FaceColor',rgb('SteelBlue'),'EdgeColor','none')
xlim([0,27]); ylim([0,25])
xlabel('SNR of HW vocalizations in dB')
ylabel('number of missed HW vocalizations')
hold off
