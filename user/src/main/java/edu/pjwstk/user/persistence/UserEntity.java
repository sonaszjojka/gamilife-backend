package edu.pjwstk.user.persistence;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.DynamicUpdate;

import java.time.LocalDate;
import java.util.UUID;

@Setter
@Getter
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "users")
@DynamicUpdate // worth considering
public class UserEntity {
    @Id
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
}
