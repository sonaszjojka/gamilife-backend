package pl.gamilife.group.util;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.group.model.ChatMessage;

import java.util.UUID;

public interface ChatMessageSpecificationBuilder {
    Specification<ChatMessage> buildSpecification(UUID groupId, Boolean isImportant);
}