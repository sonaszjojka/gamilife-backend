package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import pl.gamilife.gamification.domain.exception.InvalidGamificationOperationException;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import java.util.Objects;
import java.util.UUID;

@Getter
@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(exclude = {"statisticType"})
@Table(name = "user_statistic")
public class UserStatistic {
    @Id
    @Builder.Default
    @Column(name = "id", nullable = false)
    private UUID id = UUID.randomUUID();

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "statistic_type_id", nullable = false)
    private Integer statisticTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "statistic_type_id", nullable = false, insertable = false, updatable = false)
    @Builder.Default
    private StatisticType statisticType = null;

    @NotNull
    @Column(name = "count", nullable = false)
    @Builder.Default
    private Integer count = 0;

    public StatisticTypeEnum getStatisticTypeEnum() {
        return StatisticTypeEnum.fromId(this.statisticTypeId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        UserStatistic that = (UserStatistic) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
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