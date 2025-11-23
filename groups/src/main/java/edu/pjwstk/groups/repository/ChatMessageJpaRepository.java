package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.ChatMessage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

public interface ChatMessageJpaRepository extends JpaRepository<ChatMessage, UUID>, JpaSpecificationExecutor<ChatMessage> {
    List<ChatMessage> findWithGroupMemberByMessageIdIn(Collection<UUID> messageIds);
}
