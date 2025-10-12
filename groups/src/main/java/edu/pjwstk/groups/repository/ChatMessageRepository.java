package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.domain.ChatMessage;

public interface ChatMessageRepository {
    ChatMessage save(ChatMessage chatMessage);
}
