package erlangvm.erlgui;

import java.util.concurrent.ThreadLocalRandom;

import com.ericsson.otp.erlang.OtpConnection;

public class AutoCarSpawner extends Thread{
	
	private WindowGui gui;
	private OtpConnection connection;
	private boolean running;
	
	public AutoCarSpawner(WindowGui gui, OtpConnection connection){
		this.gui = gui;
		this.connection = connection;
		running=false;
	}
	
	public void gracefulstop(){
		running = false;
	}

	public void begin(){
		running = true;
	}
	
	public void run(){
		int startRand = 0;
		while(true){
			if(running) {
				int randomNum = ThreadLocalRandom.current().nextInt(1, 999);
				String[] starts = {"i_nord3", "i_est3", "i_ovest3", "i_sud3"};
				String[] stops = {"o_nord3", "o_est3", "o_ovest3", "o_sud3"};
				startRand = (startRand + 1) % 4;
				int stopRand = ThreadLocalRandom.current().nextInt(0, 4);
	
				int speedRand = ThreadLocalRandom.current().nextInt(200, 1000);
				
			    // display/center the jdialog when the button is pressed
				String car = "car" + randomNum;
	//			JFrame j = new JFrame(car);
	//			j.setBounds(50,50,150,150);
	//			jframemap.put(car, j);
	//			j.setVisible(true);
				
			    gui.spawn_car(connection, car, "Fiat Panda", starts[startRand], stops[stopRand], speedRand, 0, 0);
	//		    JDialog d = new JDialog(frame, "Hello", true);
	//		    d.setLocationRelativeTo(frame);
	//		    d.setVisible(true);

			}
		    try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
