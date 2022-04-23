import javax.swing.*;

public class Game extends JDialog {
    private JLabel nivelLabel;
    private JLabel vidasLabel;
    private JLabel puntuacionLabel;
    private JPanel mainPanel;
    private JButton button2;
    private JButton button4;
    private JButton button5;
    private JButton button6;
    private JButton button7;
    private JButton button8;
    private JButton button9;
    private JButton button10;
    private JButton button11;
    private JButton button12;

    public Game(String nivel, int vidas, int puntuacion) {
        setContentPane(mainPanel);
        setModal(true);
        nivelLabel.setText(nivel);
        vidasLabel.setText(String.valueOf(vidas));
        puntuacionLabel.setText(String.valueOf(puntuacion));
    }
}
