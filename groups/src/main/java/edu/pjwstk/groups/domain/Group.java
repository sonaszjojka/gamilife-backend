package edu.pjwstk.groups.domain;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group")
public class Group {

    @Id
    @Column(name = "group_id", nullable = false, updatable = false, unique = true)
    private UUID groupId;

    @Column(name = "join_code", unique = true, updatable = false, length = 20, nullable = false)
    private String joinCode;

    @Column(name = "admin_id", nullable = false)
    private UUID adminId;

    @Column(name = "group_currency_symbol", nullable = false)
    private Character groupCurrencySymbol;

    @Column(name = "members_limit", updatable = true, nullable = false)
    private Integer membersLimit;

    @ManyToOne
    @JoinColumn(name = "group_type_id", nullable = false)
    private GroupType groupType;

    @OneToMany(mappedBy = "group")
    @ToString.Exclude
    private List<ChatMessage> chatMessages;

    @OneToMany(mappedBy = "memberGroup")
    @ToString.Exclude
    private List<GroupMember> groupMembers;

    @OneToMany(mappedBy = "groupRequested")
    @ToString.Exclude
    private List<GroupRequest> groupRequests;

    @OneToMany(mappedBy = "groupInvited")
    @ToString.Exclude
    private List<GroupInvitation> groupInvitations;
}
