package pl.gamilife.task.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.Immutable;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@ToString(exclude = {"tasks", "habits"})
@Table(name = "task_difficulty")
public class TaskDifficulty extends BaseIntReadOnlyEntity {

    @Column(name = "name", nullable = false, length = 50)
    private String name;

    @Column(name = "value", nullable = false)
    private Integer value;

    @OneToMany(mappedBy = "difficulty")
    private Set<Task> tasks = new HashSet<>();

    @OneToMany(mappedBy = "difficulty")
    private Set<Habit> habits = new HashSet<>();

}
