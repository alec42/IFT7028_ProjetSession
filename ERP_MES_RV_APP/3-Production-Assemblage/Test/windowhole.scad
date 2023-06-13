module window(x_tot,y_tot,w,h,rupl_a,rupl_b,rupr_a,rupr_b,rlor_a,rlor_b,rlol_a,rlol_b){
//rupl=radius upper left
//rlor = radius lower right

//Parameters to change
//Top Left ellipse
a_1=rupl_a;
b_1=rupl_b;
//Top right ellipse
a_2=rupr_a;
b_2=rupr_b;
//Bottom right ellipse
a_3=rlor_a;
b_3=rlor_b;
//Bottom left ellipse
a_4=rlol_a;
b_4=rlol_b;
    
    
//Total height and length
height=h;
width=w;

//Do not change these params
step = 1/500;
inter_height_1=height-b_1;
inter_height_2=height-b_2;
inter_width_1=width-a_1-a_2;
inter_width_2=width-a_4-a_3;

//Creating points for shape
toplefttellipse = [for (x=[-a_1:step:0]) [x+a_1+x_tot,sqrt(b_1^2-b_1^2*(x^2)/a_1^2)+inter_height_1+y_tot]];
toprightellipse = [for (x=[0:step:a_2]) [x+inter_width_1+a_1+x_tot,sqrt(b_2^2-b_2^2*(x^2)/a_2^2)+inter_height_2+y_tot]];
bottomrightellipse = [for (x=[a_3:-step:0]) [x+inter_width_2+a_4+x_tot,-sqrt(b_3^2-b_3^2*(x^2)/a_3^2)+b_3+y_tot]];
bottomleftellipse = [for (x=[0:-step:-a_4]) [x+a_4+x_tot,-sqrt(b_4^2-b_4^2*(x^2)/a_4^2)+b_4+y_tot]];

points = concat(toplefttellipse,toprightellipse,bottomrightellipse,bottomleftellipse);

//echo(len(toplefttellipse));
//Creating shape
polygon(points);
}
window(100,100,50,70,20,20,20,20,5,5,5,5);

















