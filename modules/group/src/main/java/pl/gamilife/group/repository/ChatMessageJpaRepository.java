package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.group.model.ChatMessage;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

public interface ChatMessageJpaRepository extends JpaRepository<ChatMessage, UUID>, JpaSpecificationExecutor<ChatMessage> {
    List<ChatMessage> findWithGroupMemberByIdIn(Collection<UUID> messageIds);
}
