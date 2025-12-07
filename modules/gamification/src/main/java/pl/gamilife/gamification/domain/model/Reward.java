package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Immutable;
import pl.gamilife.shared.persistence.entity.BaseUuidReadOnlyEntity;

@Getter
@Entity
@Immutable
@SuperBuilder
@NoArgsConstructor
@Table(name = "reward")
public class Reward extends BaseUuidReadOnlyEntity {

    @NotNull
    @Column(name = "statistic_type_id", nullable = false, insertable = false, updatable = false)
    private Integer statisticTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "statistic_type_id", nullable = false)
    private StatisticType statisticType;

    @NotNull
    @Column(name = "experience", nullable = false)
    private Integer experience;

    @NotNull
    @Column(name = "money", nullable = false)
    private Integer money;

}