package pl.gamilife.user.persistence;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.DynamicUpdate;

import java.time.Instant;
import java.time.LocalDate;
import java.util.UUID;

@Setter
@Getter
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "user")
@DynamicUpdate // worth considering
public class UserEntity {
    @Id
    @Column(name = "id")
    private UUID id;

    @Column(name = "first_name", nullable = false)
    private String firstName;

    @Column(name = "last_name", nullable = false)
    private String lastName;

    @Column(name = "email", nullable = false)
    private String email;

    @Column(name = "password")
    private String password;

    @Column(name = "username", nullable = false)
    private String username;

    @Column(name = "date_of_birth")
    private LocalDate dateOfBirth;

    @NotNull
    @Column(name = "level", nullable = false)
    private int level;

    @Column(name = "experience", nullable = false)
    private int experience;

    @Column(name = "money", nullable = false)
    private int money;

    @Column(name = "send_budget_reports", nullable = false)
    private boolean sendBudgetReports;

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

    @Column(name = "timezone", nullable = false)
    private String timezone;

    @Column(name = "last_timezone_change", nullable = false)
    private Instant lastTimezoneChange;

    @Column(name = "timezone", nullable = false)
    private String timezone;

    @Column(name = "last_timezone_change", nullable = false)
    private Instant lastTimezoneChange;

}
