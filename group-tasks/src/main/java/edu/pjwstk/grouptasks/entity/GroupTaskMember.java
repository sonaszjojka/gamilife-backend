package edu.pjwstk.grouptasks.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_task_member")
public class GroupTaskMember extends AbstractEntitySuperclass {

    @Id
    @Column(name = "group_task_member_id", nullable = false)
    private UUID id;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_task_id", nullable = false)
    private GroupTask groupTask;

    @NotNull
    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @NotNull
    @Column(name = "is_marked_done", nullable = false)
    private Boolean isMarkedDone = false;

}
