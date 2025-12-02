package pl.gamilife.grouptask.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_task")
public class GroupTask  {


    @Id
    @Column(name = "group_task_id", nullable = false)
    private UUID groupTaskId;

    @NotNull
    @Column(name = "task_id", nullable = false)
    private UUID taskId;

    @NotNull
    @Column(name = "group_id", nullable = false)
    private UUID groupId;

    @Column(name = "reward")
    private Integer reward;

    @Column(name = "is_accepted")
    private Boolean isAccepted;

    @Column(name = "accepted_date")
    private Instant acceptedDate;

    @Size(max = 300)
    @Column(name = "decline_message", length = 300)
    private String declineMessage;

    @Builder.Default
    @OneToMany(mappedBy = "groupTaskId", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<GroupTaskMember> groupTaskMembers = new LinkedHashSet<>();

    @Column(name = "last_edit")
    protected Instant lastEdit;

    @PrePersist
    public void prePersist() {
        this.lastEdit = Instant.now();
    }

    @PreUpdate
    public void preUpdate() {
        this.lastEdit = Instant.now();
    }

}
