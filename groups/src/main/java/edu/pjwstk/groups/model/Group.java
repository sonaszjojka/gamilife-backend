package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.GroupTypeEnum;
import jakarta.persistence.*;
import lombok.*;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
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

    @Column(name = "group_name", length = 50, nullable = false)
    private String name;

    @Column(name = "admin_id", nullable = false)
    private UUID adminId;

    @Column(name = "group_currency_symbol", nullable = false)
    private Character groupCurrencySymbol;

    @Column(name = "members_limit", nullable = false)
    private Integer membersLimit;

    @Column(name = "group_type_id", insertable = false, updatable = false, nullable = false)
    private Integer groupTypeId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "group_type_id", nullable = false)
    private GroupType groupType;

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    @Builder.Default
    private Set<ChatMessage> chatMessages = new HashSet<>();

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    @Builder.Default
    private Set<GroupMember> groupMembers = new HashSet<>();

    @OneToMany(mappedBy = "groupRequested", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    @Builder.Default
    private Set<GroupRequest> groupRequests = new HashSet<>();

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    @Builder.Default
    private Set<GroupInvitation> groupInvitations = new HashSet<>();

    public boolean isFull() {
        return groupMembers != null && groupMembers.size() >= membersLimit;
    }

    public boolean isUserAdmin(UUID userId) {
        return adminId.equals(userId);
    }

    public boolean isOfType(GroupTypeEnum typeEnum) {
        return groupType.toEnum() == typeEnum;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Group group = (Group) o;
        return Objects.equals(groupId, group.groupId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupId);
    }
}
