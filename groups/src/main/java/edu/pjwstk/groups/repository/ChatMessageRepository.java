package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.ChatMessage;

public interface ChatMessageRepository {
    ChatMessage save(ChatMessage chatMessage);
}
