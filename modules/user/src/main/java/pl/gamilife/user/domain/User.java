package pl.gamilife.user.domain;

import lombok.*;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.*;
import java.time.temporal.ChronoUnit;
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
    private String timezone;
    @Setter(AccessLevel.NONE)
    private Instant lastTimezoneChange;

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
}
