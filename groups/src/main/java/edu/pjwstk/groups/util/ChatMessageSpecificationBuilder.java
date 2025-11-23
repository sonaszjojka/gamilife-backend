package edu.pjwstk.groups.util;

import edu.pjwstk.groups.model.ChatMessage;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface ChatMessageSpecificationBuilder {
    Specification<ChatMessage> buildSpecification(UUID groupId, Boolean isImportant);
}