package pl.gamilife.grouptask.entity;

import jakarta.persistence.*;
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
public class GroupTaskMember {

    @Id
    @Column(name = "group_task_member_id", nullable = false)
    private UUID groupTaskMemberId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_task_id", nullable = false)
    private GroupTask groupTaskId;

    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @Column(name = "is_marked_done")
    private Boolean isMarkedDone;

}
