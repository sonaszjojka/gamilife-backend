package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.Immutable;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.shared.persistence.entity.BaseUuidReadOnlyEntity;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "achievement")
@ToString(exclude = {"statisticType", "items", "userAchievements"})
public class Achievement extends BaseUuidReadOnlyEntity {

    @Size(max = 100)
    @NotNull
    @Column(name = "name", nullable = false, length = 100)
    private String name;

    @Size(max = 250)
    @NotNull
    @Column(name = "description", nullable = false, length = 250)
    private String description;

    @Size(max = 255)
    @NotNull
    @Column(name = "image_path", nullable = false)
    private String imagePath;

    @NotNull
    @Column(name = "statistic_type_id", nullable = false)
    private Integer statisticTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "statistic_type_id", nullable = false, insertable = false, updatable = false)
    private StatisticType statisticType;

    @NotNull
    @Column(name = "goal", nullable = false)
    private Integer goal;

    @NotNull
    @Column(name = "money_reward", nullable = false)
    private Integer moneyReward;

    @NotNull
    @Column(name = "experience_reward", nullable = false)
    private Integer experienceReward;

    @OneToMany(mappedBy = "achievement")
    private Set<Item> items = new HashSet<>();

    @OneToMany(mappedBy = "achievement")
    private Set<UserAchievement> userAchievements = new HashSet<>();

    public StatisticTypeEnum getStatisticTypeEnum() {
        return StatisticTypeEnum.fromId(this.statisticTypeId);
    }

}