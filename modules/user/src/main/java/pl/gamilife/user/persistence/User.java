package pl.gamilife.user.persistence;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Email;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.*;
import java.time.temporal.ChronoUnit;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "user", schema = "user")
public class User extends BaseEntity {

    @Column(name = "first_name", nullable = false)
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @Email
    @Column(name = "email", nullable = false)
    private String email;

    @Column(name = "password")
    private String password;

    @Column(name = "username", nullable = false)
    private String username;

    @Column(name = "date_of_birth")
    private LocalDate dateOfBirth;

    @Column(name = "level", nullable = false)
    private int level = 0;

    @Column(name = "experience", nullable = false)
    private int experience = 0;

    @Column(name = "money", nullable = false)
    private int money = 0;

    @Setter
    @Column(name = "send_budget_reports", nullable = false)
    private boolean sendBudgetReports;

    @Setter
    @Column(name = "is_profile_public", nullable = false)
    private boolean isProfilePublic;

    @Column(name = "is_email_verified", nullable = false)
    private boolean isEmailVerified;

    @Column(name = "is_tutorial_completed", nullable = false)
    private boolean isTutorialCompleted;

    @Column(name = "timezone", nullable = false)
    private String timezone;

    @Column(name = "last_timezone_change", nullable = false)
    private Instant lastTimezoneChange;

    private User(String firstName, String lastName, String email, String password, String username, LocalDate dateOfBirth, boolean sendBudgetReports, boolean isProfilePublic, boolean isEmailVerified, String timezone) {
        setFirstName(firstName);
        setLastName(lastName);
        setEmail(email);
        changePassword(password);
        setUsername(username);
        setDateOfBirth(dateOfBirth);
        setSendBudgetReports(sendBudgetReports);
        setProfilePublic(isProfilePublic);
        this.isEmailVerified = isEmailVerified;
        this.isTutorialCompleted = false;
        tryUpdateTimezone(timezone);
    }

    public static User create(String firstName, String lastName, String email, String password, String username, LocalDate dateOfBirth, boolean sendBudgetReports, boolean isProfilePublic, boolean isEmailVerified, String timezone) {
        return new User(firstName, lastName, email, password, username, dateOfBirth, sendBudgetReports, isProfilePublic, isEmailVerified, timezone);
    }

    public void verifyEmail() {
        if (this.isEmailVerified) {
            throw new DomainValidationException("Email already verified");
        }

        this.isEmailVerified = true;
    }

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

    public boolean tryUpdateTimezone(String timezone) {
        if (timezone == null || timezone.isBlank()) {
            throw new DomainValidationException(String.format("Invalid timezone: %s", timezone));
        }

        if (this.timezone == null) {
            this.timezone = timezone;
            this.lastTimezoneChange = Instant.now();
            return true;
        }

        ZoneId currentZoneId = ZoneId.of(this.timezone);
        ZoneId newZoneId;
        try {
            newZoneId = ZoneId.of(timezone);
        } catch (DateTimeException e) {
            throw new DomainValidationException(String.format("Invalid timezone: %s", timezone));
        }

        Instant now = Instant.now();
        ZoneOffset currentOffset = currentZoneId.getRules().getOffset(now);
        ZoneOffset newOffset = newZoneId.getRules().getOffset(now);
        if (currentOffset.equals(newOffset)) {
            this.timezone = timezone;
            return false;
        }

        if (this.lastTimezoneChange.plus(1, ChronoUnit.HOURS).isAfter(now)) {
            return false;
        }

        this.timezone = timezone;
        this.lastTimezoneChange = now;
        return true;
    }

    public void setFirstName(String firstName) {
        if (firstName == null || firstName.isBlank()) {
            throw new DomainValidationException("First name cannot be null or empty");
        }

        if (firstName.length() > 100) {
            throw new DomainValidationException("First name cannot be longer than 100 characters");
        }

        this.firstName = firstName;
    }

    public void setLastName(String lastName) {
        if (lastName == null && this.lastName == null) {
            return;
        }

        if (lastName == null || lastName.isBlank()) {
            throw new DomainValidationException("Last name cannot be null or empty");
        }

        if (lastName.length() > 100) {
            throw new DomainValidationException("Last name cannot be longer than 100 characters");
        }

        this.lastName = lastName;
    }

    public void setUsername(String username) {
        if (username == null || username.isBlank()) {
            throw new DomainValidationException("Username cannot be null or empty");
        }

        if (username.length() > 100) {
            throw new DomainValidationException("Username cannot be longer than 100 characters");
        }

        this.username = username;
    }

    public void setDateOfBirth(LocalDate dateOfBirth) {
        if (this.dateOfBirth == null && dateOfBirth == null) {
            return;
        }

        if (dateOfBirth.isAfter(LocalDate.now())) {
            throw new DomainValidationException("Date of birth cannot be in the future");
        }

        this.dateOfBirth = dateOfBirth;
    }

    public void changePassword(String password) {
        if (this.password == null && password == null) {
            return;
        }

        if (password == null || password.isBlank()) {
            throw new DomainValidationException("Password cannot be null or empty");
        }

        this.password = password;
    }

    public void completeTutorial() {
        if (this.isTutorialCompleted) {
            throw new DomainValidationException("Tutorial already completed");
        }

        this.isTutorialCompleted = true;
    }

    private void setEmail(String email) {
        if (email == null || email.isBlank()) {
            throw new DomainValidationException("Email cannot be null or empty");
        }

        if (email.length() > 320) {
            throw new DomainValidationException("Email cannot be longer than 320 characters");
        }

        this.email = email;
    }

}
