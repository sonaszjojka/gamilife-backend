package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "invitation_status")
public class InvitationStatus extends BaseIntReadOnlyEntity {

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "status", fetch = FetchType.LAZY)
    @ToString.Exclude
    private List<GroupInvitation> groupInvitations;

    public InvitationStatusEnum toEnum() {
        return InvitationStatusEnum.fromId(this.getId());
    }
}
