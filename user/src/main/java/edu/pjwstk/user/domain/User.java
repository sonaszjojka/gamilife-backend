package edu.pjwstk.user.domain;

import lombok.*;

import java.time.Instant;
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
    private int experience;
    private int money;
    private boolean sendBudgetReports;
    private boolean isProfilePublic;
    private boolean isEmailVerified;

    @Setter(AccessLevel.NONE)
    private Instant passwordChangeDate;

    public void setPassword(String password) {
        this.password = password;
        this.passwordChangeDate = Instant.now();
    }
}
