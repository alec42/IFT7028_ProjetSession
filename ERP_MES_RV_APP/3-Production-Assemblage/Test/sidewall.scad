module sidewall(){
//Parameters to change
//Left ellipse
a_1=75;
b_1=75;
//Right ellipse
a_2=75;
b_2=75;
//Total height and length
height=150;
width=250;

//Do not change these params
step = 1/500;
inter_height_1=height-b_1;
inter_height_2=height-b_2;
inter_width=width-a_1-a_2;

//Creating points for shape
toplefttellipse = [ for (x=[-a_1:step:0]) [x+a_1,sqrt(b_1^2-b_1^2*(x^2)/a_1^2)+inter_height_1]];
toprightellipse = [ for (x=[0:step:a_2]) [x+inter_width+a_1,sqrt(b_2^2-b_2^2*(x^2)/a_2^2)+inter_height_2]];
points = concat([[0,0]],toplefttellipse,toprightellipse,[[inter_width+a_1+a_2,0]]);

echo(len(toplefttellipse));
//Creating shape
//linear_extrude(height=0.25)
polygon(points);
}
sidewall();



























