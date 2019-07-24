package erlangvm.erlgui;

import java.awt.Color;
import java.awt.EventQueue;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;

import com.ericsson.otp.erlang.*;

import java.awt.GridLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.text.DefaultCaret;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ThreadLocalRandom;

public class WindowGui {

	private JFrame frame;
	private JFrame frame2;
	private JFrame frame3;

	private JTextArea textArea = new JTextArea (30,30);

	private Map<String, JFrame> jframemap = new HashMap<String, JFrame>();
	
	private static OtpConnection connection;
	/**
	 * Launch the application.
	 */
	
	int squaredim = 12;
	int baserow = 1;
	int basecolumn = 0;
	
	private Map<String, Couple> map = new HashMap<String, Couple>();
	private JLabel[][] labels = new JLabel[squaredim][squaredim];

	private Map<String, JLabel> carinfo = new HashMap<String, JLabel>();

	private int startRand = 0;
	
	private static AutoCarSpawner autospawner;
	
	public void spawn_car(OtpConnection connection, String car, String desc, String start, String stop, int speed, int prio, double faultprob) {
        OtpErlangObject[] funargs = new OtpErlangObject[7];
        funargs[0] = new OtpErlangAtom(car);
        funargs[1] = new OtpErlangString(desc);
        funargs[2] = new OtpErlangAtom(start);
        funargs[3] = new OtpErlangAtom(stop);
        funargs[4] = new OtpErlangInt(speed);
        funargs[5] = new OtpErlangInt(prio);
        funargs[6] = new OtpErlangDouble(faultprob);
        OtpErlangList funargslist = new OtpErlangList(funargs);
        try {
	        connection.sendRPC("dim_env","spawn_car",funargslist);
	        OtpErlangObject received = connection.receiveRPC(); 
	        System.out.println(received.toString());
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
        
	}


	public void updategraph(String node, List<String> cars){
		int x = map.get(node).getx();
		int y = map.get(node).gety();
		
		Iterator it = cars.iterator();
		String carsin = "";
		while(it.hasNext()){
			carsin = carsin + " ";
			carsin = carsin + it.next();
			carsin = carsin + " ";
		}
		labels[x][y].setText(carsin);
		labels[x][y].setHorizontalAlignment(SwingConstants.CENTER);
		labels[x][y].setVerticalAlignment(SwingConstants.CENTER);		
		labels[x][y].setForeground(Color.BLACK);

		
	}
	public void updatecarcolor(String node, String color){
		System.out.println("node " + node);
		int x = map.get(node).getx();
		int y = map.get(node).gety();
		
		switch(color) {
		case "green":
			labels[x][y].setForeground(Color.GREEN);
			break;
		case "red":
			labels[x][y].setForeground(Color.RED);
			break;
		case "pink":
			labels[x][y].setForeground(Color.PINK);
			break;
		}
		
		
	}
	
	public void updatecarstate(String car, String state){
		textArea.append(">> " + car + " entered in state " + state + "\n");
	}
	
	public static void main(String[] args) {
		
		OtpNode n1 = null;
		OtpSelf self = null;
		OtpPeer peer = null;
		
		String remotePeer = "node2@Altro-MB.local";
        try {
        	
	        self = new OtpSelf("javaclient", "guitar" ); 
	        peer = new OtpPeer(remotePeer);
	        n1 = new OtpNode("jv@Altro-MB.local", "guitar");

//	        App.remote  = new OtpPeer(App.remoteNode);
	        connection = self.connect(peer);
	        
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
        
        final OtpNode node = n1;

        
//        spawn_car("car3", "Fiat Panda", "i_est3", "o_sud3", 1000);

		
//        if (node.ping(remotePeer, 2000)) {
//            System.out.println(remotePeer + " is up.");
//        } else {
//            System.out.println(remotePeer + " is down");
//        }

        
//        ErlMBox receiver = new ErlMBox(node);
//        receiver.start();
        
//        receiver.gracefulstop();

        
//        spawn_car("car3", "Fiat Panda", "i_est3", "o_sud3", 1000);
//        App.spawn_car(connection, "car2", "Fiat Panda", "i_nord3", "o_sud3", 1000);
		EventQueue.invokeLater(new Runnable() {
		public void run() {
			try {
				WindowGui window = new WindowGui();
				window.frame.setVisible(true);
		        ErlMBox receiver = new ErlMBox(window, node);
		        receiver.start();
//		        AutoCarSpawner spawner = new AutoCarSpawner(window, connection);
//		        spawner.start();
		        autospawner = new AutoCarSpawner(window, connection);
		        autospawner.start();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	});


		

	}

	/**
	 * Create the application.
	 */
	public WindowGui() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBounds(0, 0, 850, 700);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel panel = new JPanel(new GridLayout(squaredim,squaredim));

		panel.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
		
		frame2 = new JFrame();
		frame2.setBounds(850, 0, 200, 200);
		frame2.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel panel2 = new JPanel();
		JButton b = new JButton("Spawn Car");
		b.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			int randomNum = ThreadLocalRandom.current().nextInt(1, 999);
			String[] starts = {"i_nord3", "i_est3", "i_ovest3", "i_sud3"};
			String[] stops = {"o_nord3", "o_est3", "o_ovest3", "o_sud3"};
			startRand = (startRand + 1) % 4;
			int stopRand = ThreadLocalRandom.current().nextInt(0, 4);
			int speedRand = ThreadLocalRandom.current().nextInt(200, 1000);
			String car = "car" + randomNum;			
		    spawn_car(connection, car, "Fiat Panda", starts[startRand], stops[stopRand], speedRand, 0, 0.8);
		  }
		});

	    b.setBounds(200,200,95,30);
	    panel2.add(b);

		JButton b2 = new JButton("AutoSpawn Start");
		b2.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			  autospawner.begin();
		  }
		});
	    b2.setBounds(200,200,95,30);
	    panel2.add(b2);

		JButton b3 = new JButton("AutoSpawn Stop");
		b3.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			  autospawner.gracefulstop();
		  }
		});
	    b3.setBounds(200,200,95,30);
	    panel2.add(b3);
	    
		JButton b4 = new JButton("Spawn Miculan");
		b4.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
				String[] starts = {"i_nord3", "i_est3", "i_ovest3", "i_sud3"};
				String[] stops = {"o_nord3", "o_est3", "o_ovest3", "o_sud3"};
				startRand = (startRand + 1) % 4;
				int stopRand = ThreadLocalRandom.current().nextInt(0, 4);	
				String car = "Miculan";
				spawn_car(connection, car, "Honda", starts[startRand], stops[stopRand], 100, 10, 0);
		  }
		});
	    b4.setBounds(200,200,95,30);
	    panel2.add(b4);
	    
	    frame2.setContentPane(panel2);
	    frame2.setVisible(true);

		frame3 = new JFrame();
		frame3.setBounds(1050, 0, 400, 600);
		frame3.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JPanel panel3 = new JPanel();
		frame3.setContentPane(panel3);
		

		JScrollPane scroll = new JScrollPane (textArea, 
		   JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		
		DefaultCaret caret = (DefaultCaret) textArea.getCaret();
		caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
		
		frame3.add(scroll);
		
		frame3.setVisible(true);

		
		for (int i =0; i<(squaredim); i++){
			for (int j =0; j<(squaredim); j++){
				labels[i][j] = new JLabel();
//				labels[i][j] = new JLabel(i + "," + j);
//				labels[i][j].setBorder(BorderFactory.createLineBorder(Color.BLACK));
			    panel.add(labels[i][j]);
			}
		}
		
		
		map.put("i_nord3", new Couple(baserow + 0, basecolumn + 4));
		map.put("i_nord2", new Couple(baserow + 1, basecolumn + 4));
		map.put("i_nord1", new Couple(baserow + 2, basecolumn + 4));
		map.put("o_nord3", new Couple(baserow + 0, basecolumn + 6));
		map.put("o_nord2", new Couple(baserow + 1, basecolumn + 6));
		map.put("o_nord1", new Couple(baserow + 2, basecolumn + 6));
		
		map.put("o_ovest3", new Couple(baserow + 3, basecolumn + 1));
		map.put("o_ovest2", new Couple(baserow + 3, basecolumn + 2));
		map.put("o_ovest1", new Couple(baserow + 3, basecolumn + 3));
		map.put("i_ovest3", new Couple(baserow + 5, basecolumn + 1));
		map.put("i_ovest2", new Couple(baserow + 5, basecolumn + 2));
		map.put("i_ovest1", new Couple(baserow + 5, basecolumn + 3));

		map.put("i_sud3", new Couple(baserow + 8, basecolumn + 6));
		map.put("i_sud2", new Couple(baserow + 7, basecolumn + 6));
		map.put("i_sud1", new Couple(baserow + 6, basecolumn + 6));
		map.put("o_sud3", new Couple(baserow + 8, basecolumn + 4));
		map.put("o_sud2", new Couple(baserow + 7, basecolumn + 4));
		map.put("o_sud1", new Couple(baserow + 6, basecolumn + 4));
		
		map.put("i_est1", new Couple(baserow + 3, basecolumn + 7));
		map.put("i_est2", new Couple(baserow + 3, basecolumn + 8));
		map.put("i_est3", new Couple(baserow + 3, basecolumn + 9));
		map.put("o_est3", new Couple(baserow + 5, basecolumn + 9));
		map.put("o_est2", new Couple(baserow + 5, basecolumn + 8));
		map.put("o_est1", new Couple(baserow + 5, basecolumn + 7));

		map.put("c_nordovest", new Couple(baserow + 3,basecolumn + 4));
		map.put("c_sudovest", new Couple(baserow + 5,basecolumn + 4));
		map.put("c_nordest", new Couple(baserow + 3,basecolumn + 6));
		map.put("c_sudest", new Couple(baserow + 5,basecolumn + 6));
		map.put("c_center", new Couple(baserow + 4,basecolumn + 5));

		map.put("c_nord", new Couple(baserow + 3,basecolumn + 5));
		map.put("c_est", new Couple(baserow + 4,basecolumn + 6));
		map.put("c_sud", new Couple(baserow + 5,basecolumn + 5));
		map.put("c_ovest", new Couple(baserow + 4,basecolumn + 4));
		
		
//		map.forEach((key,value) -> labels[value.getx()][value.gety()].setText(key));
		map.forEach((key,value) -> labels[value.getx()][value.gety()].setBorder(BorderFactory.createLineBorder(Color.BLACK)));

		map.forEach((key,value) -> labels[value.getx()][value.gety()].setToolTipText(key));

		labels[map.get("c_nordovest").getx()][map.get("c_nordovest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_sudovest").getx()][map.get("c_sudovest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED, 3));
		labels[map.get("c_nordest").getx()][map.get("c_nordest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_sudest").getx()][map.get("c_sudest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_center").getx()][map.get("c_center").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));

		labels[map.get("c_nord").getx()][map.get("c_nord").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_est").getx()][map.get("c_est").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_sud").getx()][map.get("c_sud").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));
		labels[map.get("c_ovest").getx()][map.get("c_ovest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED,3));


		
		
//		labels[map.get("c_nordovest").getx()][map.get("c_nordovest").gety()].setBackground(Color.RED);
//		labels[map.get("c_sudovest").getx()][map.get("c_sudovest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED));
//		labels[map.get("c_nordest").getx()][map.get("c_nordest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED));
//		labels[map.get("c_sudest").getx()][map.get("c_sudest").gety()].setBorder(BorderFactory.createLineBorder(Color.RED));
//		labels[map.get("c_center").getx()][map.get("c_center").gety()].setBorder(BorderFactory.createLineBorder(Color.RED));

		
		frame.setContentPane(panel);
		
//		labels[6][5].setText("name");
	}

}
