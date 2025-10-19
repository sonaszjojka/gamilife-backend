package edu.pjwstk.grouptasks.entity;

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

    @Column(name = "reward")
    private Integer reward;

    @NotNull
    @Column(name = "is_accepted", nullable = false)
    private Boolean isAccepted = false;

    @Column(name = "accepted_date")
    private Instant acceptedDate;

    @Size(max = 300)
    @Column(name = "decline_message", length = 300)
    private String declineMessage;



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
