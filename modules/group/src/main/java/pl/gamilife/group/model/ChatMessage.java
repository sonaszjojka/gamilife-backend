package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "chat_message", schema = "group")
public class ChatMessage extends BaseEntity {

    @Column(name = "content", nullable = false, updatable = false)
    private String content;

    @Column(name = "is_important", nullable = false)
    private Boolean isImportant;

    @Column(name = "group_id", nullable = false, updatable = false, insertable = false)
    private UUID groupId;

    @ToString.Exclude
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false)
    private Group group;

    @Column(name = "sender_id", nullable = false, updatable = false, insertable = false)
    private UUID groupMemberId;

    @ToString.Exclude
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sender_id", nullable = false)
    private GroupMember groupMember;

    private ChatMessage(String content, boolean isImportant, Group group, GroupMember groupMember) {
        setContent(content);
        setImportance(isImportant);
        setGroup(group);
        setGroupMember(groupMember);
    }

    public static ChatMessage create(String content, boolean isImportant, Group group, GroupMember groupMember) {
        return new ChatMessage(content, isImportant, group, groupMember);
    }

    public void setContent(String content) {
        if (content == null || content.isBlank()) {
            throw new DomainValidationException("Content cannot be null or empty");
        }

        if (content.length() > 255) {
            throw new DomainValidationException("Content cannot be longer than 255 characters");
        }

        this.content = content;
    }

    public void setImportance(boolean important) {
        isImportant = important;
    }

    public void setGroup(Group group) {
        if (group == null) {
            throw new DomainValidationException("Group cannot be null");
        }

        this.group = group;
        this.groupId = group.getId();
    }

    public void setGroupMember(GroupMember groupMember) {
        if (groupMember == null) {
            throw new DomainValidationException("Group member cannot be null");
        }

        this.groupMember = groupMember;
        this.groupMemberId = groupMember.getId();
    }
}
