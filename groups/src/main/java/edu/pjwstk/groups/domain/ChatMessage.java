package edu.pjwstk.groups.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
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

    @Column(name = "is_important", nullable = false)
    private Boolean isImportant;

    @Column(name = "send_at")
    protected Instant sendAt;

    @PrePersist
    public void prePersist() {
        this.sendAt = Instant.now();
    }

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false)
    private Group group;

}
