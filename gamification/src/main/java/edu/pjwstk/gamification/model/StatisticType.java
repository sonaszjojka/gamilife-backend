package edu.pjwstk.gamification.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.ToString;
import org.hibernate.annotations.Immutable;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Table(name = "statistic_type")
@ToString(exclude = {"userStatistics", "achievements"})
@Immutable
public class StatisticType {
    @Id
    @Column(name = "id", nullable = false)
    private Integer id;

    @Size(max = 100)
    @NotNull
    @Column(name = "type", nullable = false, length = 100)
    private String type;

    @OneToMany(mappedBy = "statisticType")
    private Set<UserStatistic> userStatistics = new HashSet<>();

    @OneToMany(mappedBy = "statisticType")
    private Set<Achievement> achievements = new HashSet<>();
}