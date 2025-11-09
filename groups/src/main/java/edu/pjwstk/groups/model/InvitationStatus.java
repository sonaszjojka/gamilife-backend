package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.InvitationStatusEnum;
import jakarta.persistence.*;
import lombok.*;

import java.util.List;
import java.util.Objects;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "invitation_status")
public class InvitationStatus {

    @Id
    @Column(name = "invitation_status_id", nullable = false, updatable = false, unique = true)
    private Integer invitationStatusId;

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "invitationStatus")
    @ToString.Exclude
    private List<GroupInvitation> groupInvitations;

    public InvitationStatusEnum toEnum() {
        return InvitationStatusEnum.fromId(this.invitationStatusId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        InvitationStatus that = (InvitationStatus) o;
        return Objects.equals(invitationStatusId, that.invitationStatusId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(invitationStatusId);
    }
}
