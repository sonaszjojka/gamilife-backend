package pl.gamilife.group.util.impl;

import pl.gamilife.group.model.ChatMessage;
import pl.gamilife.group.util.ChatMessageSpecificationBuilder;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class ChatMessageSpecificationBuilderImpl implements ChatMessageSpecificationBuilder {

    @Override
    public Specification<ChatMessage> buildSpecification(
            UUID groupId,
            Boolean isImportant) {

        return Specification.allOf(
                hasGroupId(groupId),
                hasIsImportant(isImportant)
        );
    }

    private Specification<ChatMessage> hasGroupId(UUID groupId) {
        return (root, query, cb) -> {
            if (groupId == null) {
                return null;
            }
            return cb.equal(root.get("groupId"), groupId);
        };
    }

    private Specification<ChatMessage> hasIsImportant(Boolean isImportant) {
        return (root, query, cb) -> {
            if (isImportant == null) {
                return null;
            }
            return cb.equal(root.get("isImportant"), isImportant);
        };
    }
}