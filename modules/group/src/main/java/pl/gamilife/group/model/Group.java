package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLRestriction;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.ZoneId;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group", schema = "group")
public class Group extends BaseEntity {

    @Column(name = "name", length = 50, nullable = false)
    private String name;

    @Column(name = "admin_id", nullable = false)
    private UUID adminId;

    @Column(name = "currency_symbol", nullable = false)
    private Character currencySymbol;

    @Column(name = "members_limit", nullable = false)
    private Integer membersLimit;

    @Column(name = "timezone", nullable = false)
    private String timezone;

    @Column(name = "type_id", insertable = false, updatable = false, nullable = false)
    private Integer typeId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "type_id", nullable = false)
    private GroupType type;

    @SQLRestriction("left_at IS NULL")
    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @BatchSize(size = 10)
    @ToString.Exclude
    private final Set<GroupMember> activeMembers = new HashSet<>();

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    private final Set<GroupRequest> groupRequests = new HashSet<>();

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    private final Set<GroupInvitation> groupInvitations = new HashSet<>();

    @OneToMany(mappedBy = "group", cascade = CascadeType.REMOVE, orphanRemoval = true, fetch = FetchType.LAZY)
    @ToString.Exclude
    private final Set<ChatMessage> chatMessages = new HashSet<>();

    private Group(String name, UUID adminId, Character currencySymbol, int membersLimit, ZoneId zoneId, GroupType groupType) {
        setName(name);
        setAdminId(adminId);
        setCurrencySymbol(currencySymbol);
        setMembersLimit(membersLimit);
        setTimezone(zoneId);
        setGroupType(groupType);
    }

    public static Group create(String name, UUID adminId, Character currencySymbol, int membersLimit, ZoneId zoneId, GroupType groupType) {
        return new Group(name, adminId, currencySymbol, membersLimit, zoneId, groupType);
    }

    public boolean isFull() {
        return activeMembers.size() >= membersLimit;
    }

    public boolean isUserAdmin(UUID userId) {
        return adminId.equals(userId);
    }

    public boolean isOfType(GroupTypeEnum typeEnum) {
        return type.toEnum() == typeEnum;
    }

    public void setName(String name) {
        if (name == null || name.isBlank()) {
            throw new DomainValidationException("Name cannot be null or empty");
        }

        if (name.length() > 100) {
            throw new DomainValidationException("Name cannot be longer than 100 characters");
        }

        this.name = name;
    }

    public void setAdminId(UUID adminId) {
        if (adminId == null) {
            throw new DomainValidationException("Admin id cannot be null");
        }

        this.adminId = adminId;
    }

    public void setCurrencySymbol(Character groupCurrencySymbol) {
        if (groupCurrencySymbol == null) {
            throw new DomainValidationException("Group currency symbol cannot be null");
        }

        this.currencySymbol = groupCurrencySymbol;
    }

    public void setMembersLimit(int membersLimit) {
        if (membersLimit <= 0) {
            throw new DomainValidationException("Members limit cannot be less than or equal to 0");
        }

        if (membersLimit > 100) {
            throw new DomainValidationException("Members limit cannot be greater than 100");
        }

        this.membersLimit = membersLimit;
    }

    public void setGroupType(GroupType groupType) {
        if (groupType == null) {
            throw new DomainValidationException("Group type cannot be null");
        }

        this.type = groupType;
        this.typeId = groupType.getId();
    }

    public void setTimezone(ZoneId zoneId) {
        if (zoneId == null) {
            throw new DomainValidationException(String.format("Invalid timezone: %s", timezone));
        }

        this.timezone = zoneId.getId();
    }
}
