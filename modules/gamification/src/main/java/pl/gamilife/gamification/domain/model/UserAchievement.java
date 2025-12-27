package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@ToString(exclude = {"achievement"})
@Table(name = "user_achievement", schema = "gamification")
public class UserAchievement extends BaseEntity {

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Column(name = "achievement_id", nullable = false, insertable = false, updatable = false)
    private UUID achievementId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "achievement_id", nullable = false)
    private Achievement achievement;

    private UserAchievement(UUID userId, Achievement achievement) {
        setUserId(userId);
        setAchievement(achievement);
    }

    public static UserAchievement create(UUID userId, Achievement achievement) {
        return new UserAchievement(userId, achievement);
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }

        this.userId = userId;
    }

    private void setAchievement(Achievement achievement) {
        if (achievement == null) {
            throw new IllegalArgumentException("Achievement cannot be null");
        }

        this.achievement = achievement;
        this.achievementId = achievement.getId();
    }

}