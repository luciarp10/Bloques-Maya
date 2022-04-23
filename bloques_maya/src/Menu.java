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
            launchGame("Facil", 8, 0);
        });

        mediumButton.addActionListener(e -> {
            launchGame("Medio", 10, 0);
        });

        hardButton.addActionListener(e -> {
            launchGame("Dificil", 15, 0);
        });
    }

    private void launchGame(String dificultad, int vidas, int puntuación) {
        Game game = new Game(dificultad, vidas, puntuación);
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
