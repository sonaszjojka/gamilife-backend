package edu.pjwstk.gamification.model;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
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
}