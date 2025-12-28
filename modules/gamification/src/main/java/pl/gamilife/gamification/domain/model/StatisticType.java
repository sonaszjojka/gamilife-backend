package pl.gamilife.gamification.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.Immutable;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "statistic_type", schema = "gamification")
@ToString(exclude = {"userStatistics", "achievements"})
public class StatisticType extends BaseIntReadOnlyEntity {

    @Size(max = 100)
    @NotNull
    @Column(name = "type", nullable = false, length = 100)
    private String type;

    @OneToMany(mappedBy = "statisticType")
    private Set<UserStatistic> userStatistics = new HashSet<>();

    @OneToMany(mappedBy = "statisticType")
    private Set<Achievement> achievements = new HashSet<>();

    public StatisticTypeEnum getStatisticTypeEnum() {
        return StatisticTypeEnum.fromId(this.getId());
    }

}