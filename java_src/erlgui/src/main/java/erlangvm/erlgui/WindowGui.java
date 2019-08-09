package erlangvm.erlgui;

import java.awt.Color;
import java.awt.EventQueue;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;

import com.ericsson.otp.erlang.*;

import java.awt.GridLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.DefaultCaret;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ThreadLocalRandom;

public class WindowGui {

	private static WindowGui window;
	
	private JFrame login;
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
	
	private JSlider slider;
	
	private double faultprob = 0;

	public boolean start_env(OtpConnection connection, String jguiMbox, String jguiNode) {
        OtpErlangObject[] funargs = new OtpErlangObject[2];
        funargs[0] = new OtpErlangAtom(jguiMbox);
        funargs[1] = new OtpErlangAtom(jguiNode);
        OtpErlangList funargslist = new OtpErlangList(funargs);
        
//        OtpErlangObject[] empty_funargs = new OtpErlangObject[0];
//        OtpErlangList empty_funargs_list = new OtpErlangList(empty_funargs);

        // start environment
//        try {
//	        connection.sendRPC("dim_env","go",empty_funargs_list);
//	        OtpErlangObject received = connection.receiveRPC(); 
//	        System.out.println(received.toString());
//        }
//        catch (Exception e) {
//        	e.printStackTrace();
//        }
        
        // passaggio parametri jgui
        try {
	        connection.sendRPC("dim_env","add_jgui_endpoint",funargslist);
	        OtpErlangTuple received = (OtpErlangTuple)connection.receiveRPC();
	        System.out.println(received.elementAt(0).toString()  + " --- " +received.toString());
	        if(received.elementAt(0).toString().equals("badrpc")){
	        	System.out.println("ASDSD");
	        	return false;
	        }
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
        return true;
        
	}
	
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
		
//		OtpNode n1 = null;
//		OtpSelf self = null;
//		OtpPeer peer = null;
		
		String remotePeer = "node2@Altro-MB.local";
        try {
        	
//	        self = new OtpSelf("javaclient", "guitar" ); 
//	        peer = new OtpPeer(remotePeer);
//	        n1 = new OtpNode("jv@Altro-MB.local", "guitar");

//	        App.remote  = new OtpPeer(App.remoteNode);
//	        connection = self.connect(peer);
	        
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
        
//        final OtpNode node = n1;

        
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
				window = new WindowGui();
				
//		        ErlMBox receiver = new ErlMBox(window, node);
//		        receiver.start();
//		        AutoCarSpawner spawner = new AutoCarSpawner(window, connection);
//		        spawner.start();

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
		
		login = new JFrame();
		login.setBounds(300,300,800,250);
		login.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		login.setLayout(new GridLayout(2,3,1,1));

		
//		JPanel loginPanel = new JPanel(new GridLayout(5,5,1,1));
		JLabel remoteLabel = new JLabel("<html>"+"Inserire nome nodo remoto e ip del nodo erlang"+"</html>");
		login.add(remoteLabel);
		JTextField remoteErlangNode = new JTextField("envnode");
		remoteErlangNode.setBounds(50,50,50,30);
		login.add(remoteErlangNode);
		
		JLabel at = new JLabel("@");
		at.setHorizontalAlignment(SwingConstants.CENTER);
		at.setVerticalAlignment(SwingConstants.CENTER);
		at.setFont(new Font(at.getFont().getName(), Font.PLAIN, 25));
		login.add(at);

		JTextField remoteErlangIp = new JTextField("192.168.1.100");
		remoteErlangIp.setBounds(50,50,50,30);
		login.add(remoteErlangIp);
		
		JButton go = new JButton("Start");
		login.add(go);
		
		Enumeration e;
		ArrayList<String> availableIp = new ArrayList<String>();
		try {
			e = NetworkInterface.getNetworkInterfaces();
			while(e.hasMoreElements())
			{
			    NetworkInterface n = (NetworkInterface) e.nextElement();
			    Enumeration ee = n.getInetAddresses();
			    while (ee.hasMoreElements())
			    {
			        InetAddress i = (InetAddress) ee.nextElement();
			        availableIp.add(i.getHostAddress());
			        // System.out.println(i.getHostAddress());
			    }
			}
		} catch (SocketException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		
		JLabel localLabel = new JLabel("<html>" + "Scegliere un nome qualsiasi per il nodo locale e selezionare ip locale" + "</html>");
		login.add(localLabel);
		
		JTextField localErlangNode = new JTextField("jguinode");
		localErlangNode.setBounds(50,50,50,30);
		login.add(localErlangNode);
		
		JLabel at2 = new JLabel("@");
		at2.setHorizontalAlignment(SwingConstants.CENTER);
		at2.setVerticalAlignment(SwingConstants.CENTER);
		at2.setFont(new Font(at.getFont().getName(), Font.PLAIN, 25));
		login.add(at2);
		
		JComboBox ips = new JComboBox(availableIp.toArray());
		login.add(ips);

//		JTextField localErlangIp = new JTextField("Local IP");
//		localErlangIp.setBounds(50,50,50,30);
//		login.add(localErlangIp);
		
//		JButton goo = new JButton("Let's Go!");
//		login.add(goo);
		
		


		

		
		login.setVisible(true);
		
		frame = new JFrame();
		frame.setBounds(0, 0, 850, 700);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel panel = new JPanel(new GridLayout(squaredim,squaredim));

		panel.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
		
		frame2 = new JFrame();
		frame2.setBounds(850, 0, 200, 700);
		frame2.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel panel2 = new JPanel(new GridLayout(10,1));
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
		    spawn_car(connection, car, "Fiat Panda", starts[startRand], stops[stopRand], speedRand, 0, faultprob);
		  }
		});

	    b.setBounds(200,200,95,30);
	    panel2.add(b);

        JSeparator s1 = new JSeparator(); 
        s1.setOrientation(SwingConstants.HORIZONTAL);
        panel2.add(s1);

	    
		JButton b2 = new JButton("AutoSpawn Start");

	    b2.setBounds(200,200,95,30);
	    panel2.add(b2);

		JButton b3 = new JButton("AutoSpawn Stop");
		b3.setEnabled(false);
		b3.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			  autospawner.gracefulstop();
			  b3.setEnabled(false);
			  b2.setEnabled(true);

		  }
		});
		b2.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			  autospawner.begin();
			  b2.setEnabled(false);
			  b3.setEnabled(true);
		  }
		});
	    b3.setBounds(200,200,95,30);
	    panel2.add(b3);

	   
		DefaultBoundedRangeModel model = new DefaultBoundedRangeModel(0, 0, 0, 100);
		slider = new JSlider(model);
        Hashtable<Integer, JLabel> position = new Hashtable<Integer, JLabel>();
        position.put(0, new JLabel("0%"));
        position.put(50, new JLabel("50%"));
        position.put(100, new JLabel("100%"));
		slider.setLabelTable(position);
        slider.setMinorTickSpacing(10);
        slider.setPaintTicks(true);

        JLabel status = new JLabel("Fault Probability: 0%", JLabel.CENTER);
        panel2.add(status);
        // Set the labels to be painted on the slider
        slider.setPaintLabels(true);
        slider.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                status.setText("Fault Probability: " + ((JSlider)e.getSource()).getValue() + "%" );
                double n = ((JSlider)e.getSource()).getValue();
                faultprob = ((JSlider)e.getSource()).getValue();
                autospawner.changeFaultProb(n);
            }
        });
        
		panel2.add(slider);

        JSeparator s2 = new JSeparator(); 
        s2.setOrientation(SwingConstants.HORIZONTAL);
        panel2.add(s2);
        
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

		frame3 = new JFrame();
		frame3.setBounds(1050, 0, 400, 700);
		frame3.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JPanel panel3 = new JPanel();
		frame3.setContentPane(panel3);
		

		JScrollPane scroll = new JScrollPane (textArea, 
		   JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		
		DefaultCaret caret = (DefaultCaret) textArea.getCaret();
		caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
		
		frame3.add(scroll);
		



		
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
		
		frame.setVisible(false);
	    frame2.setVisible(false);
		frame3.setVisible(false);
		go.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e)
		  {
			OtpNode node = null;
			try {
				String nodename = remoteErlangNode.getText() + "@" + remoteErlangIp.getText();
				System.out.println("NodeName is: " + nodename);
//				node = new OtpNode(nodename);
//				System.out.println("DONE1");
				
		        OtpPeer peer = new OtpPeer(nodename);
		        
		        String localnodeName = localErlangNode.getText() + "@" + ips.getSelectedItem().toString();
		        System.out.println("Local Node is: " + localnodeName);
				node = new OtpNode(localnodeName, "guitar");
//		        System.out.println("DONE2");

				
//		        System.out.println("DONE3");

//		        n1 = new OtpNode("jv@Altro-MB.local", "guitar");

//		        App.remote  = new OtpPeer(App.remoteNode);
		        OtpSelf self = new OtpSelf("javaclient", "guitar" );

		        connection = self.connect(peer);

//		        System.out.println("DONE4");
		        
				ErlMBox receiver = new ErlMBox(window, node);
				receiver.start();
		        
		    	if(!start_env(connection, "mbox", localnodeName)){
					JOptionPane.showMessageDialog(frame,
						    "Probably environment is not running on remote node.",
						    "Error",
						    JOptionPane.ERROR_MESSAGE);
					System.exit(0);
		    	}

		        autospawner = new AutoCarSpawner(window, connection);
		        autospawner.start();
			    
		        
		        frame.setVisible(true);
			    frame2.setVisible(true);
			    frame3.setVisible(true);
			    
			    login.setVisible(false);
		        
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
				JOptionPane.showMessageDialog(frame,
					    "Probably Erlang Node is Down.",
					    "Error",
					    JOptionPane.ERROR_MESSAGE);
				System.exit(0);
			} catch (OtpAuthException e1) {
				// TODO Auto-generated catch block
				JOptionPane.showMessageDialog(frame,
					    "Probably cookie auth is wrong.",
					    "Auth Error",
					    JOptionPane.ERROR_MESSAGE);
				e1.printStackTrace();
				System.exit(0);
			}

		  }
		});
//		labels[6][5].setText("name");
	}

}
