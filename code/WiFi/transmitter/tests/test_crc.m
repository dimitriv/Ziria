% $$$  
% $$$  Copyright (c) Microsoft Corporation
% $$$  All rights reserved. 
% $$$ 
% $$$  Licensed under the Apache License, Version 2.0 (the ""License""); you
% $$$  may not use this file except in compliance with the License. You may
% $$$  obtain a copy of the License at
% $$$ 
% $$$  http://www.apache.org/licenses/LICENSE-2.0
% $$$ 
% $$$  THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
% $$$  CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
% $$$  LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
% $$$  A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.
% $$$ 
% $$$  See the Apache Version 2.0 License for specific language governing
% $$$  permissions and limitations under the License.
% $$$ 
% $$$ 

Mc = {'04', '02', '00', '2e', '00', '60', '08', 'cd', '37', 'a6', '00', '20', 'd6', '01', '3c', 'f1', '00', '60', '08', 'ad', '3b', 'af', '00', '00', '4a', '6f', '79', '2c', '20', '62', '72', '69', '67', '68', '74', '20', '73', '70', '61', '72', '6b', '20', '6f', '66', '20', '64', '69', '76', '69', '6e', '69', '74', '79', '2c', '0a', '44', '61', '75', '67', '68', '74', '65', '72', '20', '6f', '66', '20', '45', '6c', '79', '73', '69', '75', '6d', '2c', '0a', '46', '69', '72', '65', '2d', '69', '6e', '73', '69', '72', '65', '64', '20', '77', '65', '20', '74', '72', '65', '61', 'da', '57', '99', 'ed'};


fprintf('Hex: ');
M = zeros(size(Mc));
for j = 1:length(Mc)
  M(j) = hex2dec(Mc{j});
  fprintf('%c', Mc{j});
end
fprintf('\n');

H = M(1:24);
P = M(25:96);
C = M(97:100);

fprintf('Str: ');
fprintf('%c', P);
fprintf('\n');


fprintf('Int8 header + payload: ');
for j = 1:96
  fprintf('%d, ', M(j));
end
fprintf('\n');


fprintf('Inverted int8 header + payload: ');
for j = 1:96
  bi = dec2bin(M(j),8);
  bi = bi(end:-1:1);
  fprintf('%d, ', bin2dec(bi));
end
fprintf('\n');


%Invert bits
fprintf('Inverted hex msg: ');
Mi = {};
for j = 1:length(Mc)
  bi = dec2bin(M(j),8);
  bi = bi(end:-1:1);
  Mi{j} = dec2hex(bin2dec(bi),2);
  fprintf('%c%c', Mi{j});
end
fprintf('\n');


fprintf('Inverted hex header + payload: ');
for j = 1:96
  fprintf('%c%c', Mi{j});
end
fprintf('\n');


fprintf('Inverted hex crc (of header + payload): ');
for j = 97:100
  fprintf('%c%c', Mi{j});
end
fprintf('\n');


% $$$ S = '0123';
% $$$ fprintf('Inverting %s: ', S);
% $$$ for j = 1:length(S)
% $$$   bi = dec2bin(S(j),8);
% $$$   bi = bi(end:-1:1);
% $$$   d = bin2dec(bi);
% $$$   fprintf('%d,', bin2dec(bi));
% $$$   % fprintf('%c%c', dec2hex(bin2dec(bi),2));
% $$$ end
% $$$ fprintf('\n');


% Calculate CRC
B = [];
for j = 1:96
  b = dec2bin(M(j), 8);
  B = [B, b];
end
B = B - '0';


ind = [0,1,3,4,6,7,9,10,11,15,21,22,25,31]+1;
X = B; %(1:32); 
C = ones(1,32);
for j=1:length(X)
  n = mod(X(j) + C(end), 2);
  C(ind) = mod(C(ind) + (zeros(size(ind))+n), 2);
  C = [n, C(1:end-1)];
end

fprintf('CRC(bin): ');
C = mod(C+1, 2);
fprintf('%d', C(end:-1:1));
fprintf('\n');

fprintf('CRC(hex): ');
for j=1:length(C)/8
  s = sprintf('%d', C((j-1)*8+1:(j-1)*8+8));
  fprintf('%c%c', dec2hex(bin2dec(s)));
end
fprintf('\n');

fprintf('CRC(dec): ');
for j=1:length(C)/8
  s = sprintf('%d', C((j-1)*8+1:(j-1)*8+8));
  d = bin2dec(s);
  if d >= 128
    fprintf('%d, ', d-256);
  else
    fprintf('%d, ', d);
  end
end
fprintf('\n');
