function [TP, FP, FN] = TPFPFNCalc(TnumStart_man, TnumStart_det, buff)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Function to calculate true positives (TP), false positives (FP) and false negatives (FN)
%TnumStart_man is the start time in julian time of manually detected calls
%TnumStart_det is the start time in julian time of automatically detected calls
%buff is the time buffer, which is applied to consider two detections to stem from the same call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TP=0;FP=0;FN=0; %initialize counters
 
dsize = size(TnumStart_det);
msize = size(TnumStart_man);

if msize(1,1) >= dsize(1,1)
    for k=1:dsize(1,1)
        currdiff=abs(TnumStart_man(:)-TnumStart_det(k));
        indtrue = find(currdiff<=datenum([0 0 0 0 0 buff]));
        if size(indtrue,1)>0
            TP = TP+1;
        else
            FP = FP+1;
        end    
    end
    FN = msize(1,1) - TP;
else
    for k=1:msize(1,1)
        currdiff=abs(TnumStart_det(:)-TnumStart_man(k));
        indtrue = find(currdiff<=datenum([0 0 0 0 0 buff]));
        if size(indtrue,1)>0
            TP = TP+1;
        else
            FN = FN+1;
        end
    end
    FP = dsize(1,1) - TP;
end

end