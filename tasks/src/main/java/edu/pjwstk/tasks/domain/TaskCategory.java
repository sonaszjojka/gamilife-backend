package edu.pjwstk.tasks.domain;

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
@Table(name = "task_category")
public class TaskCategory {

    @Id
    @Column(name = "category_id", nullable = false, updatable = false)
    private Integer id;

    @Column(name = "title", nullable = false, length = 50)
    private String title;

    @Column(name = "value", nullable = false)
    private Integer value;

    @OneToMany(mappedBy = "category")
    @ToString.Exclude
    private List<Task> tasks;
}
