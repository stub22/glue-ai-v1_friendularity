T0=transl([0.5,0.3,0])*trotx(pi);
T1=transl([0.5,-0.3,0])*trotx(pi/2);
exec('./models/mdl_puma560.sce',-1);
q0 = ikine6s(p560,T0);
q1 = ikine6s(p560,T1);
t = [0:0.05:2]';
q = jtraj(q0, q1, t);
plot(t,q(:,1),'b-',t,q(:,2),'g-.',t,q(:,3),'r--',...
   t,q(:,4),'b-+',t,q(:,5),'g-o',t,q(:,6),'r-d');
legend('q1','q2','q3','q4','q5','q6',3);
xlabel('time');
ylabel('q');
T=fkine(p560,q);
P=t2p(T);
 
rpy=tr2rpy(T);
subplot(121),plot(t,P(1,:),'b-',t,P(2,:),'g--',t,P(3,:),'r-o');
legend('X','Y','Z',3);
xlabel('time');
ylabel('XYZ coordinates of tool frame');
subplot(122),plot(t,rpy(:,1)','b-',t,rpy(:,2)','g--',t,rpy(:,3)','r-o');
legend('roll','pitch','yaw',3);
xlabel('time');
ylabel('RPY angles');

plot(P(1,:),P(2,:))
xgrid
xlabel('X');
ylabel('Y');

// cartesian trajectory
Tc = ctraj(T0, T1, length(t));
Pc = t2p(Tc);
rpyc = tr2rpy(Tc); 
subplot(121),plot(t,Pc(1,:),'b-',t,Pc(2,:),'g--',t,Pc(3,:),'r-o');
h=gca();
h.data_bounds = [0, -0.4;2 0.6];
legend('X','Y','Z',3);
xlabel('time');
ylabel('XYZ coordinates of tool frame');

subplot(122),plot(t,rpyc(:,1)','b-',t,rpyc(:,2)','g--',t,rpyc(:,3)','r-o');
legend('roll','pitch','yaw',3);
xlabel('time');
ylabel('RPY angles');

plot(Pc(1,:),Pc(2,:))
xgrid
xlabel('X');
ylabel('Y');

qc = ikine6s(p560, Tc);
plot(t,qc(:,1),'b-',t,qc(:,2),'g-.',t,qc(:,3),'r--',...
   t,qc(:,4),'b-+',t,qc(:,5),'g-o',t,qc(:,6),'r-d');
legend('q1','q2','q3','q4','q5','q6',3);
xlabel('time');
ylabel('q');

// ================singularity ======================
exec('./models/mdl_puma560.sce',-1);
T0 = transl([0.5, 0.3, 0.44])*troty(pi/2);
T1 = transl([0.5, -0.3, 0.44])*troty(pi/2);
t = [0:0.05:2]';
Ts = ctraj(T0, T1, length(t));
qs = ikine6s(p560, Ts);
plot(t,qs(:,1),'b-',t,qs(:,2),'g-.',t,qs(:,3),'r--',...
   t,qs(:,4),'b-+',t,qs(:,5),'g-o',t,qs(:,6),'r-d');
legend('q1','q2','q3','q4','q5','q6',4);
xlabel('time');
ylabel('q');

// compute manipulability
m = maniplty(p560, qs);
plot(t,m);
xlabel('time');
ylabel('manipulability');

// ============== configuration change ======================

T = transl([0.5, 0.3, 0])*trotx(pi);
 
q_r=ikine6s(p560,T, 'ru');
 
q_l = ikine6s(p560, T, 'lu');
 
q = jtraj(q_r, q_l, t);
 
plot(t,q(:,1),'b-',t,q(:,2),'g-.',t,q(:,3),'r--',...
   t,q(:,4),'b-+',t,q(:,5),'g-o',t,q(:,6),'r-d');
legend('q1','q2','q3','q4','q5','q6',4);
xlabel('time');
ylabel('q');
// this command takes time
for(i=1:41),PlotRobot(p560,q(i,:),'hold','noworld','notool'),end
