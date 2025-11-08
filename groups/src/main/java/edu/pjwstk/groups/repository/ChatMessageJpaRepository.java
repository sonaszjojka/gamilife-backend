package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.ChatMessage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ChatMessageJpaRepository extends JpaRepository<ChatMessage, UUID> {
}
