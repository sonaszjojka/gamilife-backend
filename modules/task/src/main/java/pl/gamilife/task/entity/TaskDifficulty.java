package pl.gamilife.task.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "task_difficulty")
public class TaskDifficulty {

    @Id
    @Column(name = "difficulty_id", nullable = false, updatable = false)
    private Integer id;

    @Column(name = "title", nullable = false, length = 50)
    private String title;

    @Column(name = "value", nullable = false)
    private Integer value;

    @OneToMany(mappedBy = "difficulty")
    @ToString.Exclude
    private List<Task> tasks;

}
