%%%%%
%
% Copyright (c) Microsoft Corporation
% All rights reserved. 
%
% Licensed under the Apache License, Version 2.0 (the ""License""); you
% may not use this file except in compliance with the License. You may
% obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
% CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
% LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
% A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.
%
% See the Apache Version 2.0 License for specific language governing
% permissions and limitations under the License.
%
%%%%%%


norm_shift = 8;

bpsk_mod_11a = 10720;
qpsk_mod_11a  = bpsk_mod_11a / 1.414;
qam16_mod_11a = bpsk_mod_11a / 3.162;
qam64_mod_11a = bpsk_mod_11a / 6.481;


%%% Create STS/STF sequence

sts_mod = bpsk_mod_11a * 1.472;

sts = zeros(64, 1);
sts(4+1)   = -sts_mod - i*sts_mod;
sts(8+1)   = -sts_mod - i*sts_mod;
sts(12+1)  = sts_mod + i*sts_mod;
sts(16+1)  = sts_mod + i*sts_mod;
sts(20+1)  = sts_mod + i*sts_mod;
sts(24+1)  = sts_mod + i*sts_mod;

sts(40+1) = sts_mod + i*sts_mod;
sts(44+1) = -sts_mod - i*sts_mod;
sts(48+1) = sts_mod + i*sts_mod;
sts(52+1) = -sts_mod - i*sts_mod;
sts(56+1) = -sts_mod - i*sts_mod;
sts(60+1) = sts_mod + i*sts_mod;

sts_time = zeros(160, 1);
sts_time(1:64) = ifft(sts);
sts_time(64+(1:64)) = sts_time(1:64);
sts_time(128+(1:32)) = sts_time(1:32);


  
%%% Create LTE/LTF sequence

lts = [0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, ...
       1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, ...
       0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, ...
       1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1];

lts = (lts'*2-1);


% 802.11n for debugging
ht_ltf=[1,1,-1,-1,1,1,-1,1,-1,1,1,1,1,1,1,-1,-1,1,1,-1,1,-1,1,1,1,1,1,...
        1,-1,-1,1,1,-1,1,-1,1,-1,-1,-1,-1,-1,1,1,-1,-1,1,-1,1,-1,1,1,1,1,-1,-1,-1,1,0,...
        0,0,-1,1,1,-1,1,1,-1,-1,1,1,-1,1,-1,1,1,1,1,1,1,-1,-1,1,1,-1,1,-1,1,1,1,1,1,...
        1,-1,-1,1,1,-1,1,-1,1,-1,-1,-1,-1,-1,1,1,-1,-1,1,-1,1,-1,1,1,1,1];



%%% Create 127-elements cyclic pilot sequence
pilots = [1,1,1,1,-1,-1,-1,1,-1,-1,-1,-1,1,1,-1, ...
          1,-1,-1,1,1,-1,1,1,-1,1,1,1,1,1,1, ...
          -1,1,1,1,-1,1,1,-1,-1,1,1,1,-1,1,-1, ...
          -1,-1,1,-1,1,-1,-1,1,-1,-1,1,1,1,1, ...
          1,-1,-1,1,1,-1,-1,1,-1,1,-1,1,1, ...
          -1,-1,-1,1,1,-1,-1,-1,-1,1,-1,-1,1, ...
          -1,1,1,1,1,-1,1,-1,1,-1,1,-1,-1,-1, ...
          -1,-1,1,-1,1,1,-1,1,-1,1,1,1,-1,-1, ...
          1,-1,-1,-1,1,1,1,-1,-1,-1,-1,-1,-1,-1];

pilot_mask = [1,1,1,-1]';


% Code to save capture to Ziria-compatible format
if 0
  out = [real(pkt1)'; imag(pkt1)'];
  out = out(:);
  out = round(out / max(out) * 10000);
  f = fopen('pkt1.infile', 'w');
  fprintf(f, '%d, ', out);
  fclose(f);
end

%%%% Load data
% Delta - manually detected start of packet


input = 4;
switch input

  case 1
    % Ziria generated direct input (no channel distorsions)
    pkt = load('../testbed/rx.infile');
    pkt = pkt(1:2:end) + i*pkt(2:2:end);
    pkt = transpose(pkt(1:2:end));
    delta = 0;

  case 2
    % Ziria generated captured input
    file = fopen('../tests/test_real_rx.infile');
    pkt = fread(file, inf, 'int16');
    pkt = pkt(1:2:end-1) + i*pkt(2:2:end);
    pkt = pkt(24753:65388);
    pkt = pkt(1:2:end);
    fclose(file);
    delta = 4;
    
  case 3
    % Wifi capture using BladeRF
    pkt = load('pkt1.infile');
    pkt = pkt(1:2:end)' + i*pkt(2:2:end)';
    delta = 61;
    
  case 4
    % Wifi capture using BladeRF
    pkt = load('pkt1.infile');
    pkt = pkt(1:2:end)' + i*pkt(2:2:end)';
    delta = -19;
    
  case 5
    % Weird packet. It seems to have only one LTS symbol, and than OFDM symbols right after that
    %pkt = pkt2;
    %delta = 62;

    pkt = pkt1;
    delta = -19;
    
    % 422, 549

end




%%% Plot signal in time (abs) and CP correlation (which can visually identify start of symbols) (Fig 1)

% Find CP prefix
CPc = [];
for d = 1:min(length(pkt),2000)-80
  CPc = [CPc, pkt(d+(1:16))'*pkt(d+64+(1:16))];
end
  
figure(1); clf(1)
subplot(2,1,1); hold on; plot(abs(pkt(1:min(length(pkt),2000))));
offsets = [0, 160, 32, 64, 64, repmat([16, 48, 16], 1, 20);];
yl = ylim;
x = -delta;
j = 1;
while x<length(pkt) && j <= length(offsets)
  if (x>=0)
    plot([x x], yl, 'r:');
  end
  x = x + offsets(j);
  j = j + 1;
end
xlim([0 600]);
title('OFDM symbols in time');
subplot(2,1,2); plot(abs(CPc));
title('CP correlation in time');



%%% Plot STS (correlation, abs(freq), angle(freq)) (Fig 2)

Y =[];
prepad = 100;
ppkt = [zeros(prepad,1); pkt];
len = min(length(ppkt), 2000);
for d=1:len-length(sts_time)
  Y = [Y, ppkt(d+(1:length(sts_time)))'*sts_time];
end
f = [fft(pkt(1:64)), fft(pkt(0+(1:64)))];
ind = [4:4:24, 40:4:60, 64+(4:4:24), 64+(40:4:60)] + 1;
figure(2); clf(2);
subplot(3,1,1); plot(-prepad+(1:length(Y)), abs(Y))
title('STS');
xlabel('Delay [samples]');
ylabel('Correlation');
subplot(3,1,2); plot(1:64, abs(f), 1:64, abs(sts)/max(abs(sts))*max(abs(f))*1.2); 
X=1:length(ind)-1;
xlabel('Sub-carrier');
ylabel('abs(fft(RX STS))');
subplot(3,1,3); 
hold on;
stairs(X,diff(angle(f(ind))), 'k');
%stairs(X(1:11),diff(angle(sts(ind(1:12)))) + 0.5, 'g');
plot(X, zeros(size(X))+pi, 'r:');
plot(X, zeros(size(X))-pi, 'r:');
plot(X, zeros(size(X)), 'r:');
xlabel('Sub-carrier');
ylabel('angle(fft(RX_STS))');



%%% Plot LTS correlation (Fig 3)

lts_start = 160 + 32 - delta;
ch = [fft(pkt(lts_start+(1:64))) ./ lts, fft(pkt(lts_start+64+(1:64))) ./ lts];
figure(3); clf(3);
subplot(3,1,1); plot(abs([pkt(lts_start+(1:64)), pkt(lts_start+64+(1:64))]));
title('LTS');
xlabel('Sub-carrier');
ylabel('abs(RX LTS)');
subplot(3,1,2); plot(abs(ch)); 
hold on; plot(1:64, abs(fft(pkt(lts_start + 128 + (1:64)))), 'r:'); 
xlabel('Sub-carrier');
ylabel('abs(chan estimate)');
legend('LTS1', 'LTS2', 'Data1');
subplot(3,1,3); 
hold on
plot(angle(ch));
plot(angle(lts), ':');
xlabel('Sub-carrier');
ylabel('angle(chan estimate)');
% Smooth the estimate
ch = (ch(:,1) + ch(:,2))/2;



%%% Receive data symbol (PLCP signal field)

pilot_index = 1;          % This changes across symbols
pilot_ref = pilots(pilot_index) * pilot_mask;
payload_start = lts_start + 128 + 80*0;
subset = [38:38+4, 44:44+12, 58:58+5, 1:1+5, 8:8+12, 22:22+4]+1;

% Original received data and its reshuffle
d = fft(pkt(payload_start + 16 + (1:64)));
dr = d(subset);
chr = ch(subset);

% Compensate channel
de = d ./ ch;
der = de(subset);


% Create estimation from pilots
% The goal is to find and remove trend, because of inaccurate clock sync
p = de([43,57,7,21]+1);
pche = p ./ pilot_ref;
a = mean(angle(pche(1:3)./pche(2:4)) ./ diff([43-64,57-64,7,21])');
X = (-31:32);
%pch = interp1([43-64,57-64,7, 21], pche, (-31:32), 'nearest', 'extrap');
pch = exp(-i*a*X);
pch = [pch(33:end), pch(1:32)];
pch = transpose(pch);
pchr = pch(subset);


% Compensate pilots
dc = de ./ pch;
dcr = der ./ pchr;


figure(4);clf(4);
subplot(3,1,1);
title('PLCP signal fiels - first data symbol');
plot(1:48, abs(dr), 1:48, abs(chr));
xlabel('Data sub-carrier');
ylabel('Amplitude');
legend('Received', 'Chan est');
subplot(3,1,2);
plot(1:48, abs(der), 1:48, abs(pchr), 1:48, abs(dcr));
legend('Post channel', 'Pilot est', 'Post pilots');
ylabel('Amplitude');
xlabel('Data sub-carrier');
subplot(3,1,3);
hold on;
stairs(1:48, angle(dr), 'k');
stairs(1:48, angle(der), 'b');
stairs(1:48, angle(dcr), 'r');
legend('Received', 'Post channel', 'Post pilots');
ylabel('Phase');
xlabel('Data sub-carrier');
d = d ./ ch;
figure(5);
subplot(1,3,1);
plot(real(dr), imag(dr), '.');
subplot(1,3,2);
plot(real(der), imag(der), '.');
xlim([-1.5 1.5]);
ylim([-1.5 1.5]);
subplot(1,3,3);
plot(real(dcr), imag(dcr), '.');
xlim([-1.5 1.5]);
ylim([-1.5 1.5]);

