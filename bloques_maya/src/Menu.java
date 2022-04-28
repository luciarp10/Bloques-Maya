import javax.swing.*;

public class Menu extends JDialog {
    private JButton mediumButton;
    private JButton hardButton;
    private JPanel mainPanel;
    private JButton easyButton;

    public Menu() {
        setContentPane(mainPanel);
        setModal(true);

        easyButton.addActionListener(e -> {
            // Ask to the user if he wants to play with 3 lives
            int opcion = JOptionPane.showConfirmDialog(null, "¿Quién iniciar una simulación?", "Jugador", JOptionPane.YES_NO_OPTION);

            launchGame("Facil", 8, 0, opcion);

        });

        mediumButton.addActionListener(e -> {
            int opcion = JOptionPane.showConfirmDialog(null, "¿Quién iniciar una simulación?", "Jugador", JOptionPane.YES_NO_OPTION);
            launchGame("Medio", 10, 0, opcion);
        });

        hardButton.addActionListener(e -> {
            int opcion = JOptionPane.showConfirmDialog(null, "¿Quién iniciar una simulación?", "Jugador", JOptionPane.YES_NO_OPTION);
            launchGame("Dificil", 15, 0, opcion);
        });
    }

    private void launchGame(String dificultad, int vidas, int puntuación, int opcion) {
        boolean esHumano = opcion == JOptionPane.NO_OPTION;
        System.out.println("EsHumano: " + esHumano);
        Game game = new Game(dificultad, vidas, puntuación, esHumano);
        game.pack();
        game.setLocationRelativeTo(null);
        game.setVisible(true);
        this.dispose();
    }

    public static void main(String[] args) {
        Menu dialog = new Menu();
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);
    }


}
