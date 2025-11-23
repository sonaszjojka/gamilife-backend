package edu.pjwstk.groups.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "chat_message")
public class ChatMessage {
    @Id
    @Column(name = "message_id", nullable = false, updatable = false, unique = true)
    private UUID messageId;

    @Column(name = "content", nullable = false, updatable = false)
    private String content;

    @Column(name = "sent_at")
    private Instant sentAt;

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

    @PrePersist
    public void prePersist() {
        if (this.sentAt == null) {
            this.sentAt = Instant.now();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        ChatMessage that = (ChatMessage) o;
        return Objects.equals(messageId, that.messageId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(messageId);
    }
}
