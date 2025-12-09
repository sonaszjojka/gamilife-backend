package pl.gamilife.task.entity;

import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.Immutable;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.List;

@Getter
@Entity
@Immutable
@SuperBuilder
@NoArgsConstructor
@ToString(exclude = {"tasks"})
@Table(name = "task_difficulty")
public class TaskDifficulty extends BaseIntReadOnlyEntity {

    @Column(name = "name", nullable = false, length = 50)
    private String name;

    @Column(name = "value", nullable = false)
    private Integer value;

    @OneToMany(mappedBy = "difficulty")
    private List<Task> tasks;

}
