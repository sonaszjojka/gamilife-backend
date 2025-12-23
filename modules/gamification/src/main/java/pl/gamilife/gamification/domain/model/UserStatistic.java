package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.gamification.domain.exception.InvalidGamificationOperationException;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@ToString(exclude = {"statisticType"})
@Table(name = "user_statistic")
public class UserStatistic extends BaseEntity {

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "statistic_type_id", nullable = false)
    private Integer statisticTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "statistic_type_id", nullable = false, insertable = false, updatable = false)
    private StatisticType statisticType = null;

    @NotNull
    @Column(name = "count", nullable = false)
    private Integer count = 0;

    private UserStatistic(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        setUserId(userId);
        setStatisticTypeId(statisticTypeEnum);
    }

    public static UserStatistic create(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        return new UserStatistic(userId, statisticTypeEnum);
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new IllegalArgumentException("User ID cannot be null");
        }

        this.userId = userId;
    }

    private void setStatisticTypeId(StatisticTypeEnum statisticTypeEnum) {
        if (statisticTypeEnum == null) {
            throw new IllegalArgumentException("Statistic type cannot be null");
        }

        this.statisticTypeId = statisticTypeEnum.getStatisticTypeId();
    }

    public StatisticTypeEnum getStatisticTypeEnum() {
        return StatisticTypeEnum.fromId(this.statisticTypeId);
    }

    public void incrementCounterBy(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Counter increment be greater than 0");
        }

        this.count += amount;
    }

    public void decrementCounterBy(int amount) {
        if (amount >= 0) {
            throw new IllegalArgumentException("Counter decrement must be less than 0");
        }

        this.count += amount;
    }

    public boolean updateStreakIfHigher(int newCount) {
        if (newCount <= 0) {
            throw new IllegalArgumentException("New streak value must be greater than 0");
        }

        if (getStatisticTypeEnum().isStreakStatistic()) {
            throw new InvalidGamificationOperationException("Only streak statistics could be processed with this method.");
        }

        if (newCount <= this.count) {
            return false;
        }

        this.count = newCount;
        return true;
    }
}