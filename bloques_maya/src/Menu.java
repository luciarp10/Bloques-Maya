import javax.swing.*;

public class Menu extends JDialog {
    // Componentes de swing
    private JButton mediumButton;
    private JButton hardButton;
    private JPanel mainPanel;
    private JButton easyButton;

    /**
     * Constructor de la clase, inicializa los componentes y asigna listeners a los botones
     */
    public Menu() {
        // Inicialización de los componentes
        setContentPane(mainPanel);
        setModal(true);

        // Asignamos listeners a los botones
        easyButton.addActionListener(e -> { // Si se pulsa el botón de facil
            lanzarJuego("Facil", "2:0"); // Lanzamos el juego con dificultad facil y con 2 minutos de tiempo
        });

        mediumButton.addActionListener(e -> { // Si se pulsa el botón de medio
            lanzarJuego("Medio", "5:0"); // Lanzamos el juego con dificultad media y con 5 minutos de tiempo
        });

        hardButton.addActionListener(e -> { // Si se pulsa el botón de dificil
            lanzarJuego("Dificil", "10:0"); // Lanzamos el juego con dificultad dificil y con 10 minutos de tiempo
        });
    }

    /**
     * Abre un JDialog Game con la dificultad y tiempo indicados.
     * @param dificultad Dificultad del juego {Fácil, Medio, Dificil}
     * @param tiempo Tiempo del juego {mm:ss}
     */
    private void lanzarJuego(String dificultad, String tiempo) {
        // Preguntamos al usuario si quiere jugar o prefiere que juegue la IA
        int opcion = JOptionPane.showConfirmDialog(null, "¿Quieres iniciar una simulación?", "Jugador", JOptionPane.YES_NO_OPTION);
        // En función de la opción, lanzamos el juego o la IA
        boolean esHumano = opcion == JOptionPane.NO_OPTION;

        // Creamos el juego con la dificultad, el tiempo y el modo indicados
        Game game = new Game(dificultad, tiempo, esHumano);
        // Mostramos el juego
        game.pack();
        game.setLocationRelativeTo(null);
        game.setVisible(true);
        // Cerramos el menú
        this.dispose();
    }

    /**
     * main de la clase, crea un menú y lo muestra
     * @param args Argumentos de la línea de comandos
     */
    public static void main(String[] args) {
        // Creamos el menú
        Menu dialog = new Menu();
        // Mostramos el menú
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);
    }
}
