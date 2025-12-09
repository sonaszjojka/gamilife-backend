package pl.gamilife.task.domain.model;

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
@Table(name = "task_category")
public class TaskCategory extends BaseIntReadOnlyEntity {

    @Column(name = "name", nullable = false, length = 50)
    private String name;

    @Column(name = "value", nullable = false)
    private Integer value;

    @OneToMany(mappedBy = "category")
    private List<Task> tasks;
}
