clear
% x,y Daten vom simulierten IT Glied laden
[x,y] = textread('xy.txt');
% Fit mit Polynom bis zum Grad 6
grad = 6;
p = polyfit(x,y,grad);
% Umbauen in symbolisches Polynom in t
syms t
f = sum( p .* t .^(grad:-1:0) )
% Laplace transformation des symbolischen Polynoms
Fs = laplace(f)
