%Read ground truth data and change to datenum format
GT = readtable('/Users/eschall/Desktop/SelectionTableTestDataSetLFDCS.txt','Delimiter','\t','ReadVariableNames',true);
%GT.datetime = datetime(GT.BeginDateTime,'InputFormat','yyyy/MM/dd HH:mm:ss.SSS');
%GT.matnum = datenum(GT.datetime); 

%%%%%%%%%%%%%%%%% Only in case Raven datetime format has errors
dinfo = dir('/Volumes/LFDCS/TestDatasetLFDCS_2');
files = {dinfo.name};
files = {files{1,find(cell2mat({dinfo.bytes})>5000)}};
Paths_in = {dinfo.folder};%Create list of path names
Paths_in = {Paths_in{1,find(cell2mat({dinfo.bytes})>5000)}};
file_list = table('Size',[length(files),2],'VariableTypes',{'string','double'},...
    'VariableNames',{'file','duration'});
for i = 1:length(files)
    file_list.file(i) = files{1,i};
    aud = audioinfo([Paths_in{1,i} '/' files{1,i}]);
    file_list.duration(i) = aud.Duration;
end 
for i = 1:height(GT)
   secs = GT.BeginTime_s_(i,1) - sum(file_list.duration(1:(find(strcmp(file_list.file,GT.BeginFile{i}))-1)));
   dt = strsplit(GT.BeginFile{i},'_');
   dt = datetime(dt{1},'InputFormat','yyyyMMdd-HHmmss');
   GT.datetime(i) = dt + seconds(secs);
   GT.matnum(i) = datenum(GT.datetime(i)); 
end
%%%%%%%%%%%%%%%%%%%%%%%% 

%Define buffer for detection comparison
buff1 = 0.7; %in seconds for deletion of doubled detections
buff2 = 1.8; %in seconds for TP estimation

%Remove manual picks with same start time (+/- 0.7s) to prevent overrepresentation    
    for i = 1:height(GT)
       ind = find(GT.BeginTime_s_(:,1) >= (GT.BeginTime_s_(i,1)-buff1) & GT.BeginTime_s_(:,1) <= (GT.BeginTime_s_(i,1)+buff1));
       if ind == i
           if i == height(GT)
                break
           else
               continue
           end
       else
           [q,I] = max(GT.Quality(ind));
           GT(ind(I),:) = [];
           if i == height(GT)
                break
           end
       end   
    end


%Read paramfiles                
paramfile = fileread('/Users/eschall/Projects/Detectors/lfdcs/process/paramfiles/LFDCS-parameter-file_PO0.txt');
%split to lines                
paramfile_l = splitlines(paramfile);
%Extract parameter names and values
param_names = {};
params = {};
ind=1;
for i = 1:length(paramfile_l)
    str = cell2mat(regexprep(paramfile_l(i,1),';.*',''));
    str = strsplit(str(~isspace(str)),':');
    if length(str)> 1
    param_names{ind,1} = str{1};
    params{ind,1} = str{2};
    ind=ind+1;
    end
end
params{4} = '12/15/10 06:16:45';

%Define paramter ranges and vector of parameter indices for parameter file
%positions
P15 = (700:200:2500); 
P17 = (0.5:0.05:0.98);
P24 = (5:5:50);
P25 = (5:5:50);
P26 = (5:5:50);
P28 = (30:7:93);
P29 = (20:30:290);
P30 = (1.5:1.5:15);
P31 = (1:1:10);
P32 = (1:1:10);
P33 = (0.2:0.1:1.1);
P34 = (5:5:50);
P35 = (1.5:1.5:15);
P36 = (10:10:100);
P37 = (50:50:500);
P38 = (0.1:0.1:1);
P39 = (1.5:1.5:15);
P40 = (5:5:50);
P41 = (5:5:50);
P42 = (0.20:0.05:0.65);
P43 = (1.5:1.5:15);
P44 = (0.20:0.05:0.65);
P45 = (1:1:10);

indices = [15,17,24,25,26,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45];

%empty table for detector efficiency values
efficiency = table('Size',[1000,4],'VariableTypes',{'double','double','double','double'},...
    'VariableNames',{'Run','TP','FP','FN'});

%empty count and vector for error run count 
ecount = 0;
erros = [];

for x = 1:1200 %Run optimization cycle 1000 times

%Optimize parameters based on detector efficiency 
    %Read detector results, convert to datenum and remove detections with same start time (+/- 1s) to
    %not count harmonics separately, keep track with highest MDist&SNR
    %product
    if exist(['/Users/eschall/Desktop/PO_Results/PO' num2str(x-1) '.csv'], 'file')
    detects = readtable(['/Users/eschall/Desktop/PO_Results/PO' num2str(x-1) '.csv'],'Delimiter',',','HeaderLines', 23);
        if height(detects) >= 1
            sDate = repmat(datetime(2010,12,15,06,16,45),height(detects),1);
            detects.dateTime = sDate + seconds(detects.Var2);
            detects.matnum = datenum(detects.dateTime);   

            for i = 1:height(detects)
               if i >= height(detects)
                        break
               end

               ind = find(detects.Var2 >= (detects.Var2(i)-buff1) & detects.Var2 <= (detects.Var2(i)+buff1));
               if ind == i
                  continue
               else
                   for j = 1:length(ind)
                       prods(j) = detects.Var8(ind(j)) * detects.Var9(ind(j));
                   end
                   [~,I] = max(prods);
                   detects(ind(ind~=I),:) = [];
                   if i == height(detects)
                        break
                   end
               end   
            end
    
            %%%%%%%%%%%%%%%%%
            %Compare detector results & ground truth  for loop of Mdists ~ calculate TP, FP and FN ~ and save values for MDists that produce FN value that is clostest to 25% missed calls
            %thres_cat = (0.5:0.5:8); %MDist categories
            %thres_eff = table('Size',[size(thres_cat,2),4],'VariableTypes',{'double','double','double','double'},...
            %'VariableNames',{'Threshold','TP','FP','FN'}); %empty table for
            %threshold result storage

            %for j = 1:size(thres_cat,2)
            %   detects_f = detects(detects.Var9 <= thres_cat(1,j),:);
            %   thres_eff.Threshold(j) = thres_cat(1,j);
            %    [thres_eff.TP(j),thres_eff.FP(j),thres_eff.FN(j)] =  TPFPFNCalc(GT.matnum,detects_f.matnum,buff2);
            %    thres_eff.Prec = thres_eff.TP./height(detects_f);
            %    thres_eff.Rec = thres_eff.TP./height(GT);
            %    thres_eff.Ratio = thres_eff.Rec./thres_eff.Prec;
            %end 
            %efficiency.Thres(x) = thres_eff.Threshold(ind); % only for evaluation
            %of different thresholds
            %[~,ind] = min(abs((thres_eff.FN/height(GT))-0.25)); %find Mdist category closest to 25% missed calls
            %efficiency.TP(x) = thres_eff.TP(ind);
            %efficiency.FP(x) = thres_eff.FP(ind);
            %efficiency.FN(x) = thres_eff.FN(ind);
            %%%%%%%%%%%%%%%%%%
    
            %Compare detector results and ground truth for Mdist threshold of 3.5
            efficiency.Run(x) = x-1;
            %detects_f = detects(detects.Var9 <= 3.5,:); %only necessary if results not already filtered in LFDCS
            [efficiency.TP(x),efficiency.FP(x),efficiency.FN(x)] =  TPFPFNCalc(GT.matnum,detects.matnum,buff2);
        else
            efficiency.Run(x) = x-1;
            efficiency.TP(x) = -2;  %LFDCS produced 0 detections at threshold 3.5
        end
    else
       efficiency.Run(x) = x-1;
       efficiency.TP(x) = -1; %LFDCS failed and did not produce output
    end    
    
    %Save efficiency table to file
    writetable(efficiency,'efficiency.csv');
    
    %Increase or decrease parameters based on detector efficiency
    %????
    
%Edit parameter values by random choice
rs = randi(10,23,1);
new_params = {[convertStringsToChars([compose("%.0f  ",P15(rs(1))),compose("%.2f  ",P17(rs(2))),compose("%.1f  ",P24(rs(3))),compose("%.1f  ",P25(rs(4))),...
    compose("%.1f  ",P26(rs(5))),compose("%.1f  ",P28(rs(6))),compose("%.1f  ",P29(rs(7))),compose("%.1f  ",P30(rs(8))),...
    compose("%.1f  ",P31(rs(9))),compose("%.1f  ",P32(rs(10))),...
    compose("%.1f  ",P33(rs(11))),compose("%.1f  ",P34(rs(12))),compose("%.1f  ",P35(rs(13))),compose("%.1f  ",P36(rs(14))),...
    compose("%.1f  ",P37(rs(15))),compose("%.1f  ",P38(rs(16))),compose("%.1f  ",P39(rs(17))),compose("%.1f  ",P40(rs(18))),...
    compose("%.1f  ",P41(rs(19))),compose("%.2f  ",P42(rs(20))),compose("%.1f  ",P43(rs(21))),compose("%.2f  ",P44(rs(22))),...
    compose("%.1f  ",P45(rs(23)))])]};
params(indices) = new_params{1};


%Write parameter file
for i = 1:length(param_names)
ind = strfind(paramfile,param_names{i});
ind = ind + length(param_names{i}) + 2;
paramfile(ind:(ind-1+length(params{i}))) = params{i};
end

fileID = fopen(['/Users/eschall/Projects/Detectors/lfdcs/process/paramfiles/LFDCS-parameter-file_PO' num2str(x) '.txt'],'w');
fprintf(fileID,'%s \n',paramfile);
fclose(fileID);

%Run LFDCS with new parameter file
p1 = [' ''paramfiles/LFDCS-parameter-file_PO' num2str(x) '.txt'''];
p2 = [' ''/Users/eschall/Desktop/PO_Results/PO' num2str(x) '.csv'''];
status = dos(['tcsh ''/Users/eschall/Documents/MATLAB/run_idl.sh''' p1 p2]);

%count error runs
if status ~= 0
ecount = ecount+1;
errors(ecount) = x;
end
        
end
