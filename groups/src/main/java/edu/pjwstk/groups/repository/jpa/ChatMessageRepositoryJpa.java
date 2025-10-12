package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.domain.ChatMessage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ChatMessageRepositoryJpa extends JpaRepository<ChatMessage, UUID> {
}
