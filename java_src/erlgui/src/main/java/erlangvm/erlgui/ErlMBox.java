package erlangvm.erlgui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class ErlMBox extends Thread {
	
	private OtpNode node;
	private OtpMbox mbox;
	private boolean running;
	private WindowGui gui;
	
	public ErlMBox(WindowGui gui, OtpNode node){
		this.node = node;
		this.gui = gui;
		mbox = node.createMbox();
		mbox.registerName("mbox");
		running = true;
	}
	
	public void gracefulstop(){
		running = false;
	}

	
	public void run(){
		
	    OtpErlangObject o;
	    OtpErlangTuple msg;
	    OtpErlangPid from;
	    
	    String msgtype;
	    OtpErlangTuple payload;
	    
	    while (running) {
//	    	System.out.println("Heya");
	    	try {
				o = mbox.receive();
//				System.out.println(o.toString());
				if (o instanceof OtpErlangTuple) {
					msg = (OtpErlangTuple)o;
					msgtype = ((OtpErlangAtom)(msg.elementAt(0))).toString();
					payload = (OtpErlangTuple)(msg.elementAt(1));
					
					switch(msgtype) {
					case "undirgraph":
						String nodeid;
						OtpErlangList graph = (OtpErlangList) payload.elementAt(0);
						Iterator graph_it = graph.iterator();
						OtpErlangTuple value;
						List<String> cars = new ArrayList<>();
						while(graph_it.hasNext()){
							value = (OtpErlangTuple) graph_it.next();
//							System.out.println(value.toString());
							nodeid = value.elementAt(0).toString();
							
							OtpErlangList erl_cars = (OtpErlangList) value.elementAt(1);
							Iterator cars_it = erl_cars.iterator();
							cars.clear();
							while(cars_it.hasNext()){
								cars.add((cars_it.next()).toString());
							}
							
//							System.out.println("nodeid is " + nodeid + ". cars are " + cars.toString());
							gui.updategraph(nodeid, cars);

						}
						System.out.println("------------------------------------");
						break;
					case "carstate":
						
						OtpErlangTuple cardataotp = (OtpErlangTuple) payload.elementAt(1);
						OtpErlangAtom carname = (OtpErlangAtom) cardataotp.elementAt(2);
						OtpErlangAtom stateotp = (OtpErlangAtom) payload.elementAt(0);
//						System.out.println("Cardata: " + cardataotp.toString());
						gui.updatecarstate(carname.toString(), stateotp.toString());
						break;
					case "carcolor":
						String color = ((OtpErlangAtom) payload.elementAt(0)).toString();
						cardataotp = (OtpErlangTuple) payload.elementAt(1);
						System.out.println("Cardata: " + cardataotp.toString());
						OtpErlangList route = (OtpErlangList) cardataotp.elementAt(4);
						System.out.println(route.toString());
						OtpErlangTuple mypostuple = (OtpErlangTuple) route.elementAt(0);
						String mypos = ((OtpErlangAtom) mypostuple.elementAt(0)).toString();
						gui.updatecarcolor(mypos, color);
						break;
					default:
						System.out.println("Unknown message");
					}
					
//					mbox.send(from,msg.elementAt(1));
				}
//				System.out.println(o.toString());
	    	}
	    	catch (Exception e) {
	    		e.printStackTrace();
	    	}
	    }
	}
}
