package pl.gamilife.user.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;
import java.util.UUID;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class User {
    private UUID id;
    private String firstName;
    private String lastName;
    private String email;
    private String password;
    private String username;
    private LocalDate dateOfBirth;
    private int level;
    private int experience;
    private int money;
    private boolean sendBudgetReports;
    private boolean isProfilePublic;
    private boolean isEmailVerified;
    private boolean isTutorialCompleted;

    public void grantExperience(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Experience granted must be greater than zero");
        }

        this.experience += amount;
    }

    public void grantMoney(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Money granted must be greater than zero");
        }

        this.money += amount;
    }

    public void levelUp(int newLevel) {
        if (newLevel <= level) {
            throw new IllegalArgumentException("New level must be greater than current level");
        }

        this.level = newLevel;
    }
}
