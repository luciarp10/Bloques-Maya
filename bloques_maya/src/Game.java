import scala.collection.immutable.List;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

public class Game extends JDialog {
    private JLabel nivelLabel;
    private JLabel vidasLabel;
    private JLabel puntuacionLabel;
    private JPanel mainPanel;
    private JPanel board;
    private JLabel puntuacionTotLabel;
    private List<List<Object>> tablero;
    private int puntuacion_acumuluda = 0;
    private ArrayList<JButton> botones;
    private boolean esHumano;

    private int[] nivel_to_int(String nivel){
        int[] niveles = new int[3];

        switch (nivel) {
            case "Facil":
                niveles = new int[]{9,11,1};
                break;
            case "Medio":
                niveles = new int[]{12,16,2};
                break;
            case "Dificil":
                niveles = new int[]{25,15,3};
                break;
        }
        return niveles;
    }

    private void generarTablero(String nivel) {
        int[] nivel_int = nivel_to_int(nivel);

        int filas = nivel_int[0];
        int columnas = nivel_int[1];

        botones = new ArrayList<>();

        board.removeAll();
        board.setLayout(new GridLayout(filas, columnas));

        this.tablero = main.generar_tablero(nivel_int[2]);
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                JButton button = new JButton();
                button.setName("button_" + i + "_" + j); //Para identificarlos al pulsarlos y saber la posición
                button.setBackground(get_color(main.obtener_posicion(tablero, i, j)));
                button.setBorder(BorderFactory.createLineBorder(Color.BLACK));
                button.addActionListener(e -> {
                    button.setBackground(Color.BLACK);
                    button.setBorder(BorderFactory.createLineBorder(Color.BLACK));

                    String[] posicion = button.getName().split("_");
                    int fila = Integer.parseInt(posicion[1]);
                    int columna = Integer.parseInt(posicion[2]);

                    List<Object> coords = main.create_list(fila, columna);

                    List<List<Object>> nuevo_tablero = main.pulsar_bloque(this.tablero, main.obtener_columna(coords,0), main.obtener_columna(coords,1));
                    puntuacionLabel.setText(String.valueOf(puntuacion_acumuluda+main.actualizar_puntuacion(tablero, nuevo_tablero, Integer.valueOf(puntuacionLabel.getText()), coords)));
                    this.tablero = main.desplazar_bloques(nuevo_tablero);
                    if (puntuacionLabel.getText().equals("0")) {
                        vidasLabel.setText(Integer.valueOf(vidasLabel.getText()) -1 + "");
                    }

                    if (Integer.valueOf(vidasLabel.getText()) == 0) {
                        JOptionPane.showMessageDialog(null, "La partida ha finalizado con una puntuación de " + puntuacionTotLabel.getText() + " puntos");

                        this.dispose();
                        Menu menu = new Menu();
                        menu.pack();
                        menu.setLocationRelativeTo(null);
                        menu.setVisible(true);


                    }
                    actualizar_tablero();

                    if (main.tablero_vacio(this.tablero)) {
                        puntuacion_acumuluda += Integer.valueOf(puntuacionLabel.getText());
                        puntuacionTotLabel.setText(Integer.parseInt(puntuacionTotLabel.getText())+puntuacion_acumuluda + "");
                        puntuacion_acumuluda=0;
                        puntuacionLabel.setText("0");
                        generarTablero(nivel);
                    }
                });
                botones.add(button);
                board.add(button);
            }
        }
    }

    private Color get_color(int n)
    {
        Color color_dev = Color.BLACK;
        switch (n) {
            case 0:
                color_dev = Color.BLACK;
                break;
            case 1:
                color_dev = Color.BLUE;
                break;
            case 2:
                color_dev = Color.RED;
                break;
            case 3:
                color_dev = Color.ORANGE;
                break;
            case 4:
                color_dev = Color.GREEN;
                break;
            case 5:
                color_dev = Color.LIGHT_GRAY;
                break;
            case 6:
                color_dev = Color.MAGENTA;
                break;
            case 7:
                color_dev = Color.GRAY;
                break;
            case 8:
                color_dev = Color.WHITE;
                break;
        }
        return color_dev;
    }

    private void actualizar_tablero()
    {
        int filas = this.tablero.length();
        int columnas = tablero.head().length();

        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                JButton button = (JButton) board.getComponent(i * columnas + j);
                button.setBackground(get_color(main.obtener_posicion(tablero, i, j)));
            }
        }
    }

    private void juego_IA()
    {
        int filas = this.tablero.length();
        int columnas = tablero.head().length();

        while (!this.vidasLabel.getText().equals("0")) {
            if (main.tablero_vacio(this.tablero)) {
                continue;
            }

            // Select random button
            int random_button;
            int fila;
            int columna;
            JButton button;
            int cont = 0;
            do {
                fila = (int) (Math.random() * filas);
                columna = (int) (Math.random() * columnas);
                random_button = fila * columnas + columna;
                button = (JButton) board.getComponent(random_button);
                cont ++;
                System.out.println("Cont: " + cont);
            } while (button.getBackground().equals(Color.BLACK) || (main.contar_iguales(this.tablero, fila, columna, main.create_empty_matrix()).length() <= 2 && cont < 100));

            // Click on button
            button.doClick();

            // Wait for animation
           /*try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }*/
        }
    }

    public Game(String nivel, int vidas, int puntuacion, boolean esHumano) {
        setContentPane(mainPanel);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        this.esHumano = esHumano;
        nivelLabel.setText(nivel);
        vidasLabel.setText(String.valueOf(vidas));
        puntuacionLabel.setText(0 + "");
        puntuacionTotLabel.setText(String.valueOf(puntuacion));

        generarTablero(nivel);

        if (!esHumano) {
            // Juego IA en otro hilo
            Thread thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    juego_IA();
                }
            });
            thread.start();
        }
    }

}
