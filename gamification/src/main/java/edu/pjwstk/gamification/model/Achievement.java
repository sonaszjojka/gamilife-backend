package edu.pjwstk.gamification.model;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.ToString;
import org.hibernate.annotations.Immutable;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@ToString(exclude = {"statisticType", "items", "userAchievements"})
@Table(name = "achievement")
@Immutable
public class Achievement {
    @Id
    @Column(name = "id", nullable = false)
    private UUID id;

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

    @Column(name = "statistic_type_id", nullable = false)
    private Integer statisticTypeId;

    @NotNull
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

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Achievement that = (Achievement) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}