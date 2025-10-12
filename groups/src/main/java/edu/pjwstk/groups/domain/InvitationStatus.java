package edu.pjwstk.groups.domain;

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
@Table(name = "invitation_status")
public class InvitationStatus {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "invitation_status_id", nullable = false, updatable = false, unique = true)
    private Integer groupTypeId;

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "invitationStatus")
    @ToString.Exclude
    private List<GroupInvitation> groupInvitations;
}
